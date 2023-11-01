import sys
import os

# Function to prepend '0' to each line in the file until it has 8 hex digits
def preprocess_mem_hex(input_file):
    try:
        output_file = input_file.split('.')[0] + "_processed.hex"
        with open(input_file, 'r') as infile:
            lines = infile.read().splitlines()
            while len(lines) < 256:
                lines.append('00000000')
            with open(output_file, 'w') as outfile:
                for line in lines:
                    line = line.strip()  # Remove leading/trailing whitespace
                    # Count the number of existing hex digits
                    hex_digits = len(line)
                    # Prepend '0' until there are 8 hex digits
                    if hex_digits < 8:
                        line = '0' * (8 - hex_digits) + line
                    formatted_line = line[6:] + ' ' + line[4:6] + ' ' + line[2:4] + ' ' + line[0:2]
                    # formatted_line = formatted_line[::-1]
                    outfile.write(formatted_line + '\n')
                outfile.write('00' + '\n')
            
        print(f"Processed file '{input_file}' and saved to '{output_file}'")
    except:
        print(f"An error occurred while processing the file '{input_file}'")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python preprocess_mem_hex.py input_file.hex")
    else:
        input_file = sys.argv[1]
        if os.path.isfile(input_file):
            preprocess_mem_hex(input_file)
        else:
            print(f"File '{input_file}' does not exist")
