# Declassiflow

**A Static Analysis for Modeling Non-Speculative Knowledge to Relax Speculative Execution Security Measures**

Rutvik Choudhary, Alan Wang, Zirui Neil Zhao, Adam Morrison, Chris W. Fletcher

---

Declassiflow is a pass written for LLVM that determines what variables are directly leaked or can be deduced in a non-speculative execution.

This repo was initially created by forking from the [llvm-tutor repo](https://github.com/banach-space/llvm-tutor). The code there was used as a starting point.

## BEFORE YOU DO ANYTHING ELSE

### Installing Spack

For the sake of explicitly managing LLVM version and making sure all dependencies are nicely contained, we'll use [Spack](https://github.com/spack/spack) to install LLVM version 16.

To download Spack, run
```
git clone -c feature.manyFiles=true https://github.com/spack/spack.git
```

The `feature.manyFiles=true` option makes git commands a bit faster to use (though we're not gonna be doing anything with the repo anyways).

From now on, I'll use `<SPACK>` to refer to the folder that contains your clone of the Spack repo.

To add Spack to the PATH of the current shell, run the following command,
```
source <SPACK>/share/spack/setup-env.sh
```

This will let you run `spack` commands.

### Installing LLVM with Spack

Now to install LLVM 16, run this command,

```
spack install -v llvm@16 ~gold build_type=Debug
```

The `-v` enables verbose output, which is helpful for making sure that progress is happening.

The `~gold` option disables the gold plugin when building LLVM. As of last writing this, [Spack fails to bootstrap LLVM with the gold plugin enabled](https://github.com/spack/spack/issues/29350).

We need to build the `Debug` version of LLVM because our pass requires it.

:hourglass_flowing_sand: Spack will build all the dependencies for LLVM and then LLVM itself. **This takes quite a while!** So go and grab lunch or something to kill time.

After LLVM has built, you need to load it by running,
```
spack load llvm@16
```

This will add the LLVM binaries, libraries, and includes to the various paths of the current shell.

### Installing yaml-cpp with Spack

[yaml-cpp](https://github.com/jbeder/yaml-cpp) is the library used to serialize the output of the tool.

To install it, we will again use Spack. The installation and loading procedure is similar to the last section. All the commands are presented here

```
spack install -v yaml-cpp@0.6
spack load yaml-cpp@0.6
```

:warning: Do **NOT** install version 0.7 of yaml-cpp. As of last writing this, [certain CMake variables are not set which leads to build issues](https://github.com/jbeder/yaml-cpp/issues/774). While [this has been fixed in a merged PR](https://github.com/jbeder/yaml-cpp/pull/1077), this PR hasn't made it into the spack package. So until this has been fixed, stick with version 0.6.

### Making Your Life Easier

Remember, **every time you start a new shell, you need to run the following commands,**
```
source <SPACK>/share/spack/setup-env.sh
spack load llvm@16
spack load yaml-cpp@0.6
```

If you forget to do this **none of the provided helper scripts will work!**

If you find this annoying, add those commands to the profile for your shell (e.g. `~/.bash_profile` if you're using bash).

## Working With Declassiflow

From here on out, I'll use `<DECLASSIFLOW>` to refer to the folder that contains your clone of the Declassiflow repo.

### Building

Just run the `<DECLASSIFLOW>/compile.sh` script.

The result of the build is the library file `<DECLASSIFLOW>/build/lib/libdeclassiflow.so`.

### Running

**(:warning: This section is a work in progress)**

To run declassiflow against an **LLVM IR file** called `<INPUT_FILE>`, use this command,
```
opt -load-pass-plugin <DECLASSIFLOW>/build/lib/libdeclassiflow.<LIB_EXT> -passes=declassiflow -disable-output <INPUT_FILE>
```

Note, if you're on Linux then `<LIBEXT>` is `so` while on Mac OS it'd be `dylib`.

:information_source: Ideally the `opt` binary should be the one installed by Spack. You can run `which opt` to make sure that the binary is located somewhere in `<SPACK>`. However, whatever `opt` binary you use will _probably_ be fine.

:bulb: The tool will print out the full analysis in YAML. To analyze it, you'll probably wanna redirect stdout to an output file `<OUTPUT_FILE>`. You can do this by adding `> <OUTPUT_FILE>` to the end of the command.

