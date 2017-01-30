package freecli
package config

import cats.data.NonEmptyList

import api.Action
import freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[Action, NonEmptyList[ConfigParsingError], A]
  type ConfigParsingErrors = NonEmptyList[ConfigParsingError]
}
