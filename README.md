# FreeCLI

[![Build Status](https://api.travis-ci.org/pavlosgi/freecli.png?branch=master)](https://travis-ci.org/pavlosgi/freecli)

### Overview

FreeCLI is another command line argument parsing library build using Free Applicative hence the name.

The library uses Cats and Shapeless at it's core. 

Here is a list of all of FreeCLI's modules:

 * `freecli-circe`: provides bindings for Circe
 * `freecli-core`: the core FreeCLI library, the types and type classes
 * `freecli-examples`: a few examples on how to use the library
 * `freecli-testkit`: testing helpers for testing the library

### Preview
Before diving in, this is what your CLI configuration will produce:

![preview](preview.png)

### Getting Started

```scala
 import pavlosgi.freecli.core.all._     // provides core operations
 import pavlosgi.freecli.command.all._  // provides command operations
 import pavlosgi.freecli.config.all._   // provides config operations
```

### Core 

##### Syntax
* `des` construct description
* `group[T]` group to generic T
* `groupT` group to tuple

### Arguments

Allows defining positional arguments

##### Syntax
* `string` argument of type String
* `int` argument of type Int
* `long` argument of type Long
* `bool` argument of type Boolean
* `file` argument of type java.io.File
* `existentFile` argument of type ExistentFile
* `newFile` argument of type NewFile
* `arg[T]` argument with type T
* `name` construct argument name
* `-~` modifier for optional configuration of the arguments

##### Parsing
* `parseArgument`
* `parseArgumentOrFail`

##### Examples
```scala
import pavlosgi.freecli.core.all._
import pavlosgi.freecli.argument.all._
 
string 
string -~ name("arg1")
string -~ name("arg1") -~ des("argument description")
 
case class Arguments(arg1: String, arg2: Int)
group[Arguments] {
  string :: 
  int
}
```  

More examples can be found in [Argument tests](./core/src/test/scala/pavlosgi/freecli/argument/ArgumentDslTest.scala) and the [Argument example](./examples/src/main/scala/pavlosgi/freecli/examples/arguments/DatabaseConfig.scala) that can be run as follows:

```
$ sbt
[info] Set current project to freecli-root (in build file:/Users/pavlos/Workspace/freecli/)
 
> project freecli-examples
[info] Set current project to freecli-examples (in build file:/Users/pavlos/Workspace/freecli/)
 
> run 8080 host username password database
[warn] Multiple main classes detected.  Run 'show discoveredMainClasses' to see the list
 
Multiple main classes detected, select one to run:
 
 [1] pavlosgi.freecli.examples.arguments.DatabaseConfig
 [2] pavlosgi.freecli.examples.command.Git
 [3] pavlosgi.freecli.examples.config.DatabaseConfig
 [4] pavlosgi.freecli.examples.options.DatabaseConfig
 
Enter number: 1
 
[info] Running pavlosgi.freecli.examples.arguments.DatabaseConfig 8080 host username password database
DatabaseConfig(8080,host,username,password,database)
[success] Total time: 22 s, completed 22-Jan-2017 23:52:26
```

#### Options

Allows defining named options

##### Syntax
* `string` option of type String
* `int` option of type Int
* `long` option of type Long
* `double` option of type Double
* `boolean` option of type Boolean
* `file` option of type File
* `existentFile` option of type ExistentFile 
* `newFile` option of type NewFile
* `flag` flag option
* `help` adds help option that prints help (does not appear in parsed type)
* `version` adds version option that prints the version (does not appear in parsed type)
* `value` adds the string value to display for version
* `opt[T]` option of type T
* `sub[T]` subset of options
* `subT(description: Description)` subset of options with description 
* `req` required option
* `or[T](default: T)` sets option default
* `--"name"` sets the name of the option 
* `-'a'` sets the name abbreviation of the option
* `-~` modifier for optional configuration of the options

##### Parsing
* `parseOption`
* `parseOptionOrFail`

##### Examples
```scala
import pavlosgi.freecli.core.all._
import pavlosgi.freecli.option.all._
 
string --"opt1"
string --"opt1" -~ des("option description")
string -'o'
string -'o' -~ des("option description")
string --"opt1" -'o'
string --"opt1" -'o' -~ des("option description")
 
case class Options(
  opt1: Option[String], 
  opt2: Int, 
  opt3: Int, 
  opt4: Boolean)
 
group[Options] {
  string  --"opt1"          ::
  int     --"opt2" -~ req   ::
  int     --"opt3" -~ or(1) ::
  flag    --"opt4" ::
  help    --"help" ::
  version --"version" -~ value("v1.0")
}
```  

More examples can be found in [Option tests](./core/src/test/scala/pavlosgi/freecli/option/OptionDslTest.scala) and the [Option example](./examples/src/main/scala/pavlosgi/freecli/examples/options/DatabaseConfig.scala) that can be run as follows:

```
$ sbt
[info] Set current project to freecli-root (in build file:/Users/pavlos/Workspace/freecli/)
 
> project freecli-examples
[info] Set current project to freecli-examples (in build file:/Users/pavlos/Workspace/freecli/)
 
> run --port 8080 --host host --username username --password password --database database
[warn] Multiple main classes detected.  Run 'show discoveredMainClasses' to see the list
 
Multiple main classes detected, select one to run:
 
 [1] pavlosgi.freecli.examples.arguments.DatabaseConfig
 [2] pavlosgi.freecli.examples.command.Git
 [3] pavlosgi.freecli.examples.config.DatabaseConfig
 [4] pavlosgi.freecli.examples.options.DatabaseConfig
 
Enter number: 4
 
[info] Running pavlosgi.freecli.examples.options.DatabaseConfig --port 8080 --host host --username username --password password --database database
DatabaseConfig(8080,host,username,password,database)
[success] Total time: 22 s, completed 22-Jan-2017 23:52:26
```

#### Config

Config allows mixing options with arguments as long as the arguments come last. 

##### Syntax
The syntax for config re-exports Argument syntax and Option syntax and resolves the conflicts between them by namespacing Option with O for the following:
 
* `O.string`
* `O.int`
* `O.long`
* `O.double`
* `O.boolean`
* `O.file`
* `O.existentFile`
* `O.newFile`

##### Parsing
* `parseConfig`
* `parseConfigOrFail`

##### Examples
```scala
import pavlosgi.freecli.core.all._
import pavlosgi.freecli.config.all._
 
O.string --"opt1"
O.string --"opt1" -~ des("option description")
O.string -'o'
O.string -'o' -~ des("option description")
O.string --"opt1" -'o'
O.string --"opt1" -'o' -~ des("option description")
 
case class Config(
  opt1: Option[String], 
  opt2: Int, 
  opt3: Int, 
  opt4: Boolean,
  arg1: String,
  arg2: Int)
 
group[Config] {
  O.string --"opt1"          ::
  O.int    --"opt2" -~ req   ::
  O.int    --"opt3" -~ or(1) ::
  flag     --"opt4"          ::
  string                     ::
  int
}
```  

More examples can be found in [Config tests](./core/src/test/scala/pavlosgi/freecli/config/ConfigDslTest.scala) and the [Config example](./examples/src/main/scala/pavlosgi/freecli/examples/config/DatabaseConfig.scala) that can be run as follows:

```
$ sbt
[info] Set current project to freecli-root (in build file:/Users/pavlos/Workspace/freecli/)
 
> project freecli-examples
[info] Set current project to freecli-examples (in build file:/Users/pavlos/Workspace/freecli/)
 
> run --port 8080 -d -v host username password database
[warn] Multiple main classes detected.  Run 'show discoveredMainClasses' to see the list
 
Multiple main classes detected, select one to run:
 
 [1] pavlosgi.freecli.examples.arguments.DatabaseConfig
 [2] pavlosgi.freecli.examples.command.Git
 [3] pavlosgi.freecli.examples.config.DatabaseConfig
 [4] pavlosgi.freecli.examples.options.DatabaseConfig
 
Enter number: 3
 
[info] Running pavlosgi.freecli.examples.config.DatabaseConfig --port 8080 -d -v host username password database
DatabaseConfig(8080,true,true,host,username,password,database)
[success] Total time: 22 s, completed 22-Jan-2017 23:52:26
```

#### Command

Allows building commands or nested commands with configurations. When nesting commands
it's important to note that the run configuration of the nested command needs to be
the product of the parent configuration and the nested command configuration.

##### Syntax
* `cmd` construct a command
* `takes` add a config to the command
* `takesG[T]` add a config as T
* `takesT` add a config as a tuple
* `runs` specify parameterless function that executes when running the command
* `runs[T]` specify function that takes T and executes when running the command

##### Parsing
* `parseCommand`
* `parseCommandOrFail`

##### Examples
```scala
import pavlosgi.freecli.core.all._
import pavlosgi.freecli.config.all._
import pavlosgi.freecli.command.all._
 
case class Command1Config(opt1: Option[Int], opt2: String)
case class Command2Config(opt3: Int, arg1: String)
case class Command3Config(arg2: String)
 
cmd("command1") {
  takesG[Command1Config] {
    O.help   --"help" ::
    O.int    --"opt1" -~ des("opt1 description") ::
    O.string --"opt2" -~ req
  } ::
  cmd("command2") {
    takesG[Command2Config] {
      O.help --"help" ::
      O.int  --"opt3" -~ or(1) ::
      string -~ name("arg1")
    } ::
    runs[ParentWith[Command1Config, Command2Config]] { conf =>
      println(conf)
    }
  } ::
  cmd("command3") {
    takesG[Command3Config] {
      O.help --"help" ::
      string -~ name("arg2")
    } ::
    runs[ParentWith[Command1Config, Command3Config]] { conf =>
      println(conf)
    }
  }
}

```

More examples can be found in [Command tests](./core/src/test/scala/pavlosgi/freecli/command/CommandDslTest.scala) and the [Git example](./examples/src/main/scala/pavlosgi/freecli/examples/command/Git.scala) that can be run as follows:

```
$ sbt
[info] Set current project to freecli-root (in build file:/Users/pavlos/Workspace/freecli/)
 
> project freecli-examples
[info] Set current project to freecli-examples (in build file:/Users/pavlos/Workspace/freecli/)
 
> run git remote add origin git@github.com:pavlosgi/freecli.git
[warn] Multiple main classes detected.  Run 'show discoveredMainClasses' to see the list
 
Multiple main classes detected, select one to run:
 
 [1] pavlosgi.freecli.examples.arguments.DatabaseConfig
 [2] pavlosgi.freecli.examples.command.Git
 [3] pavlosgi.freecli.examples.config.DatabaseConfig
 [4] pavlosgi.freecli.examples.options.DatabaseConfig
 
Enter number: 2
 
[info] Running pavlosgi.freecli.examples.command.Git git remote add origin git@github.com:pavlosgi/freecli.git
Remote origin git@github.com:pavlosgi/freecli.git added
[success] Total time: 22 s, completed 22-Jan-2017 23:52:26
```

## License

freecli is licensed under the **[Apache License, Version 2.0][apache]** (the
"License"); you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Copyright 2015-2017 Pavlos Georgiou

[apache]: http://www.apache.org/licenses/LICENSE-2.0