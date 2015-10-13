def remove_tix(program)
  File.delete "#{program}.tix" if File.exist? "#{program}.tix"
end

def report(program)
  report = `hpc report --include=Main #{program}`
  report_lines = report.split("\n")

  expressions = report_lines[0]
  expressions = expressions[(expressions.index('(') + 1)...(expressions.index(')'))].split('/')

  alternatives = report_lines[5]
  alternatives = alternatives[(alternatives.index('(') + 1)...(alternatives.index(')'))].split('/')

  {
    expressions: {covered: expressions[0], total: expressions[1]},
    alternatives: {covered: alternatives[0], total: alternatives[1]}
  }
end

def covered?(info)
  info[:expressions][:covered] == info[:expressions][:total] &&
    info[:alternatives][:covered] == info[:alternatives][:total]
end

def improved?(prev_info, info)
  info[:expressions][:covered] > prev_info[:expressions][:covered] ||
    info[:alternatives][:covered] > prev_info[:alternatives][:covered]
end

program = ARGV[0]
if program
  # Compile program with hpc
  `ghc -fhpc #{program}.hs`
  unless $?.success?
    abort "Compilation failed"
  end

  # Remove existing tix file
  remove_tix program

  # Get examples
  puts `./#{program}`
  info = report(program)
  while !covered?(info) do
    example = `./#{program}`
    prev_info = info
    info = report(program)
    if improved?(prev_info, info)
      puts example
    end
  end

  # Clean up
  remove_tix program
else
  puts "No file provided"
end
