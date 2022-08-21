# Importing Lexer:
import krev

# Starting shell:
print("You are using Krev 0.0.1-dev.")
print("========================== <filename>.kr ==========================\n\n")

# Shell loop:
while True:
    line = input(":~ ")
    result, error = krev.run('<stdin>', line)

    if error: 
        print(error.asString())
    else: 
        print(result)
