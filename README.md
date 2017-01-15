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

##### Examples
```scala
import pavlosgi.freecli.core.all._
import pavlosgi.freecli.argument.all._
 
string 
string -~ name("arg1")
string -~ name("arg1") -~ des("argument description")
 
case class Config(arg1: String, arg2: Int)
group[Config] {
  string :: 
  int
}
```  

More examples can be found in [Argument tests](./core/src/test/scala/pavlosgi/freecli/argument/ArgumentDslTest.scala) and the [Argument example](./examples/src/main/scala/pavlosgi/freecli/examples/arguments/DatabaseConfig.scala)

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
* `help` adds help option that prints help
* `opt[T]` option of type T
* `sub[T]` subset of options
* `subT(description: Description)` subset of options with description 
* `req` required option
* `or[T](default: T)` sets option default
* `--"name"` sets the name of the option 
* `-'a'` sets the name abbreviation of the option
* `-~` modifier for optional configuration of the options

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
 
case class Config(
  opt1: Option[String], 
  opt2: Int, 
  opt3: Int, 
  opt4: Boolean)

group[Config] {
  string --"opt1"          ::
  int    --"opt2" -~ req   ::
  int    --"opt3" -~ or(1) ::
  flag   --"opt4"
}
```  

More examples can be found in [Option tests](./core/src/test/scala/pavlosgi/freecli/option/OptionDslTest.scala) and the [Option example](./examples/src/main/scala/pavlosgi/freecli/examples/options/DatabaseConfig.scala)

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

More examples can be found in [Config tests](./core/src/test/scala/pavlosgi/freecli/config/ConfigDslTest.scala) and the [Config example](./examples/src/main/scala/pavlosgi/freecli/examples/config/DatabaseConfig.scala)

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

More examples can be found in [Command tests](./core/src/test/scala/pavlosgi/freecli/command/CommandDslTest.scala) and the [Git example](./examples/src/main/scala/pavlosgi/freecli/examples/command/Git.scala)

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