fd ".zig" | entr -s "echo \"---start---\"; zig test ./interpreter.zig; echo \"---end---\""
