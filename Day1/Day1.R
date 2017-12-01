# Get the input
input = readChar("Day1/Input1_1.txt", nchars = file.size("Day1/Input1_1.txt"))

# Split it into a vector of numbers
input = as.numeric(strsplit(input, "", fixed = T)[[1]])
input_len = length(input)

###################### PART 1 ######################
# We want to compare the elements in position 1:n against the element
  # in position (2:n,1). This can be achieved doing (1:n - 1 + 1) %% n + 1
sum(input[input == input[seq_len(input_len) %% input_len + 1]])

###################### PART 2 ######################
# We want to compare the elements in position 1:n against the element
  # n/2 forward. This can be achieved doing (1:n - 1 + n/2) %% n + 1
sum(input[input == input[(seq_len(input_len) + (input_len / 2) - 1) %% input_len + 1]])
