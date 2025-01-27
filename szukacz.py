import os


def collect_code_files(directory, output_file):
    """
    Recursively searches for .hs and .erl files in the given directory,
    and writes their contents along with filenames into the output file.
    """
    with open(output_file, "w", encoding="utf-8") as out_file:
        for root, _, files in os.walk(directory):
            for file in files:
                if file.endswith(".hs"):
                    file_path = os.path.join(root, file)
                    try:
                        with open(file_path, "r", encoding="utf-8") as f:
                            content = f.read()
                        out_file.write(f"===== FILE: {file_path} =====\n")
                        out_file.write(content + "\n\n")
                    except Exception as e:
                        print(f"Error reading {file_path}: {e}")


if __name__ == "__main__":
    input_directory = "C:/Users/barte/OneDrive/Pulpit/Studia/ROK III/PF/RubikSolver"
    output_file = "output.txt"
    collect_code_files(input_directory, output_file)
    print(f"Collected files saved to {output_file}")
