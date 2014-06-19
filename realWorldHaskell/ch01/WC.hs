-- counts the number of words in a file
-- runghc WC to run
main = interact wordCount
       where wordCount input = show(length (words input)) ++ "\n"