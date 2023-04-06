package gorgonartisan

import scala.annotation.experimental
import scala.util.Try

import cats.Show
import cats.data.OptionT
import cats.effect.IO
import cats.implicits.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.latestbit.circe.adt.codec.JsonTaggedAdt
import scribe.cats.{io => log}

type Key = Content.Id

trait Data

trait Content extends Data {
  val Name: String
  val InternalName: String
  val Description: String
}

type ContentProcessed = ItemProcessed | RecipeProcessed | QuestProcessed

object Content {
  case class Id(value: Int)
  object Id {
    def fromKey(key: String): Either[String, Id] =
      Either.fromOption(
        key.split("_").last.toIntOption.map(Id.apply),
        show"Failed to parse key $key",
      )
  }

  type Ref[A <: Content] = Map[Id, A] => Option[A]
  // type InameRef[A <: Content] = String => Map[Id, A] => Option[A]

  def byId[A <: Content](id: Int): Ref[A] = _.get(Id(id))
  def byIname[A <: Content](iname: String): Ref[A] =
    _.values.find(_.InternalName == iname)
}

// trait Processed[+C]

trait Info extends Data
object Info {
  type Ref[A <: Info] = Map[String, A] => Option[A]
  def byKey[A <: Info](k: String): Ref[A] = _.get(k)
  def byNpcName(name: String): Ref[Npc]   = _.get(name.split('/').last)
}

case class ItemStack(
    ChanceToConsume: Option[Float],
    Desc: Option[String],
    ItemCode: Option[Int],
    ItemKeys: Option[List[String]],
    StackSize: Int,
)

case class ItemStackProcessed(
    ChanceToConsume: Option[Float],
    Desc: Option[String],
    ItemCode: Option[Content.Ref[Item]],
    ItemKeys: Option[List[String]],
    StackSize: Int,
) extends Processed[ItemStack]

case class Area(
    FriendlyName: String,
    ShortFriendlyName: Option[String],
) extends Info

case class AreaProcessed(
    FriendlyName: String,
    ShortFriendlyName: Option[String],
) extends Processed[Area]

case class Npc(
    AreaName: String,
    AreaFriendlyName: String,
    Preferences: Option[List[Npc.Preference]],
    Name: String,
) extends Info

object Npc {
  case class Preference(
      Keywords: List[String], // refs Keyword
      Pref: Float,
  )
}

case class NpcProcessed(
    area: Info.Ref[Area],
    Preferences: Option[List[Npc.Preference]],
    Name: String,
) extends Processed[Npc]

case class Source(
    Type: String,
    Npc: Option[String],
    SkillTypeId: Option[String],
    ItemTypeId: Option[Int],
) extends Info

case class SourceProcessed(
    Type: String,
    Npc: Option[Info.Ref[Npc]],
    SkillTypeId: Option[Info.Ref[Skill]],
    ItemTypeId: Option[Content.Ref[Item]],
) extends Processed[Source] //{
//   extension (source: Source) {
//     def process = SourceProcessed(
//       source.Type,
//       source.Npc map Info.byKey[Npc],
//       source.SkillTypeId map Info.byKey[Skill],
//       source.ItemTypeId map Content.byId[Item],
//     )
//   }
// }

case class Ai() extends Info

enum ContentFile(val name: String) {
  case Abilities         extends ContentFile("abilities")
  case AdvancementTables extends ContentFile("advancementtables")
  case Ai                extends ContentFile("ai")
  case Areas             extends ContentFile("areas")
  case Attributes        extends ContentFile("attributes")
  case DirectedGoals     extends ContentFile("directedgoals")
  case Effects           extends ContentFile("effects")
  case Items             extends ContentFile("items")
  case ItemUses          extends ContentFile("itemuses")
  case LorebookInfo      extends ContentFile("lorebookinfo")
  case Lorebooks         extends ContentFile("lorebooks")
  case Npcs              extends ContentFile("npcs")
  case PlayerTitles      extends ContentFile("playertitles")
  case Quests            extends ContentFile("quests")
  case Recipes           extends ContentFile("recipes")
  case Skills            extends ContentFile("skills")
  case AbilitySources    extends ContentFile("sources_abilities")
  case RecipeSources     extends ContentFile("sources_recipes")
  case StorageVaults     extends ContentFile("storagevaults")
  case TreasureEffects   extends ContentFile("tsysclientinfo")
  case XpTables          extends ContentFile("xptables")
}

// @experimental
object ContentFile {
  given Show[ContentFile] = _.name

  val infoListFiles = List(AbilitySources, RecipeSources)

  extension (file: ContentFile) {
    def getReader: String => Either[io.circe.Error, Map[
      Content.Id,
      Processed[Content],
    ]] = // ContentReader[C, R] =
      file match {
        case ContentFile.Items   => readContent[Item]
        case ContentFile.Recipes => readContent[Recipe]
        case ContentFile.Quests  => readContent[Quest]
        case _                   => readContent[Item]
      }
  }
}

// type InfoListFile = ContentFile.AbilitySources | ContentFile.RecipeSources

// type DataFile = ContentFile | InfoListFile

// case class Id(value: String)
// object Id {
//   given ReadWriter[Id] = stringKeyRW(
//     readwriter[String].bimap[Id](_.value, Id.apply)
//   )
// }

type JsonReader[A] = String => Either[io.circe.Error, Map[String, A]]
type ContentReader[C <: Content, R <: Processed[C]] =
  String => ContentReaderResult[C, R]
type ContentReaderResult[C <: Content, R <: Processed[C]] =
  Either[io.circe.Error, Map[Content.Id, R]]
// type InfoReader                  = JsonReader[Info]
// type InfoListReader              = JsonReader[List[Info]]

// def decodeMap[A: Decoder] = decode[Map[String, A]]

def getContentReader[C <: Content: Decoder]: JsonReader[C] =
  decode[Map[String, C]]
// file match {
//   case ContentFile.Items   => decode[Map[String, C]]
//   case ContentFile.Recipes => decodeMap[Recipe]
//   case _                   => decodeMap[Item]
// }

// def getInfoListReader(file: ContentFile): InfoListReader =
//   file match {
//     case ContentFile.RecipeSources  => decodeMap[List[Source]]
//     case ContentFile.AbilitySources => decodeMap[List[Source]]
//     case _                          => decodeMap[List[Source]]
//   }

def indexesToIds[C <: Content]: Map[String, C] => Map[Content.Id, C] =
  _.map((id, content) =>
    Content.Id.fromKey(id) tupleRight content
  ).logLeftCollectRight.toMap

def setRefs[C <: Content](using
    ProcessData[C]
): Map[Content.Id, C] => Map[Content.Id, Processed[C]] =
  _.view.mapValues(_.process).toMap

def readContent[C <: Content: Decoder](json: String)(using
    ProcessData[C]
): Either[io.circe.Error, Map[Content.Id, Processed[C]]] =
  // explicit apply is required for correct implicit resolution
  getContentReader.apply(json).map(indexesToIds).map(setRefs)

// def readFile[C <: Content: Decoder, R <: Processed[C]](using
//     ProcessData[C, R]
// )(file: ContentFile): Either[ReadFileError, Map[Content.Id, R]] =
//   file match {
//     case ContentFile.Items =>
//       readContent[Item, ItemProcessed].rightWiden[Map[Content.Id, R]]
//     case ContentFile.Recipes => readContent[Recipe, RecipeProcessed]
//     case _                   => InvalidContentFile(file).asLeft
//   }

type ReadFileError = InvalidContentFile | io.circe.Error

case class InvalidContentFile(val file: ContentFile, val msg: String)
    extends Exception

// @experimental
object InvalidContentFile {
  def apply(file: ContentFile): InvalidContentFile =
    new InvalidContentFile(file, show"Invalid ContentFile: $file")
}
