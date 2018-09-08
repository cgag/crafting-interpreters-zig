fd ".zig" | entr -s "echo \"---start---\"; zig test ./parser.zig; echo \"---end---\""
