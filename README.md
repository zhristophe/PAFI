# PAFI
PAFI: A Tool for Path Feasibility Analysis

## Usage

| Option       | Description                    |
|--------------|--------------------------------|
| -h, --help   | Show this help message.        |
| -c, --code   | Enable code output.            |
| -a, --ast    | Enable AST output.             |
| -m, --model  | Enable model output.           |
| -t, --time   | Enable time output.            |
| other        | The path of input file.        |

For example:

```bash
./pafi.exe array.cpath -c -m
```

## Grammar

See [here](mini-C.g4). We write it in the g4 file format, which can be accepted by ANTLR4.
