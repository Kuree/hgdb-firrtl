HGDB Symbol Table for Chisel/Firrtl
===================================

This is a proof-of-concept Firrtl pass that extracts debugging information from the
Firrtl file and converts it into a hgdb supported format. Notice that this pass
*does not* change your Firrtl design.

# Usage
First we need to build the `jar` file so that `firrtl` can recognize our custom pass
```
$ ./bin/install [firrtl_path]
```
where `[firrtl_path]` is where `Firrtl` folder is. It assumes that you have build the
Firrtl using `sbt assembly` command. The `install` file will generate a new `firrtl`
file in the `./bin` folder that has proper java class path set.

Then we need to convert your Firrtl file into a hgdb symbol table:

```
$ ./bin/firrtl <your_firrtl_file> -hgdb <toml_output>
$ pip install hgdb[all] # if you haven't done that already
$ toml2hgdb <toml_output> debug.db
```

We can use `debug.db` as the symbol table for debugging. Below shows an example of debugging Chisel3
example code with the symbol table we just generated:

![Image of Debugging Chisel](https://github.com/Kuree/files/blob/master/images/chisel3-vscode.png?raw=true)

You can use any supported debugger hosted [here](https://github.com/Kuree/hgdb-debugger).

# How does it work?
This pass analyzes the assignment and conditional (if) statements and uses Firrtl embedded `FileInfo` to
create the symbol table. Explicitly named signals will be converted into `Generator Variables`.

Below are a list of limitations:
1. Because Firrtl doesn't track symbol changes, we have to make some assumptions on how high-level
   signals are lowered. The assumption works well for most cases, but may break in the future.
2. Chisel does not capture frame/context information when producing Firrtl. As a result, there will not
   be any "local" variables shown when you debug.
3. Because the pass has to work on the IR before the mux pulling pass, it can only operates on high-form IR.
   Therefore, if there is any downstream optimization that removes the symbol,
   you'll see undefined symbols when you debug.
   Ideally Firrtl should provide `-O0` switch when compile to RTL that disable all the optimization passes.


# Want to learn more about hgdb?
hgdb is designed to be a flexible debugging framework for hardware generators that supports any mainstream
RTL simulators. You can check out the technical details [here](https://github.com/Kuree/hgdb/tree/master/docs).
