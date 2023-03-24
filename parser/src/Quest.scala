package gorgonartisan

import cats.implicits.*
import io.circe.ACursor
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import io.circe.Json
import io.circe.Json.Folder
import io.circe.generic.auto.*
import org.latestbit.circe.adt.codec.JsonTaggedAdt

case class Quest(
    Description: String,
    DisplayedLocation: Option[String],
    FavorNpc: Option[String],
    FollowUpQuests: Option[List[String]],
    GroupingName: Option[String],
    InternalName: String,
    IsAutoPreface: Option[Boolean],
    IsAutoWrapUp: Option[Boolean],
    IsCancellable: Option[Boolean],
    Keywords: Option[List[String]],
    Level: Option[Int],
    MidwayText: Option[String],
    Name: String,
    NumExpectedParticipants: Option[Int],
    Objectives: List[Quest.Objective],
    PrefaceText: Option[String],
    PreGiveEffects: Option[List[String]],
    PreGiveRecipes: Option[List[String]],
    Requirements: Option[List[Quest.Requirement]],
    RequirementsToSustain: Option[List[Quest.Requirement]],
    ReuseTime_Minutes: Option[Int],
    ReuseTime_Hours: Option[Int],
    Reward_Favor: Option[Int],
    Rewards: Option[List[Quest.Reward]],
    Rewards_Effects: Option[List[String]],
    Rewards_Items: Option[List[ItemStack]],
    Rewards_NamedLootProfile: Option[String],
    SuccessText: Option[String],
    TSysLevel: Option[Int],
    Version: Int,
) extends Content

object Quest {
  case class Objective(
      Description: String,
      Number: Option[Int],
      GroupId: Option[Int],
      IsHiddenUntilEarlierObjectivesComplete: Option[Boolean],
      Requirements: Option[List[Requirement]],
      Type: Objective.Types,
  )

  object Objective {
    sealed trait Types // can't be called Type due to name collision
    object Types {
      case class Collect(
          ItemName: Option[String], // item
          Target: Option[String],   // keyword?
      ) extends Types

      case class Scripted() extends Types

      case class ScriptedReceiveItem(
          Item: String,  // Item
          Target: String,// Npc with area
      ) extends Types

      case class ScriptAtomicInt(Target: String) extends Types
      //
//
      case class Kill(
          AbilityKeyword: Option[String], // Keyword
          Target: String,                 // Ai, Keyword(?), "*", "Area:${Area}"
          area: Option[String],
      ) extends Types

      case class KillElite(
          Target: String // Enemy keyword or "*"
      ) extends Types

      case class Harvest(
          ItemName: Option[String], // Item
          Target: Option[String],   // Item? Keyword?
      ) extends Types

      case class Loot(
          ItemName: Option[String],      // Item
          Target: Option[String],        // Keyword
          MonsterTypeTag: Option[String],// Enemy keyword?
      ) extends Types

      case class BeAttacked(
          AnatomyType: Option[String],
          Target: Option[String], // Ai
      ) extends Types

      case class Bury(
          AnatomyType: Option[String],
          Target: Option[String], // Ai
      ) extends Types

      case class Deliver(
          ItemName: String,             // Item
          NumToDeliver: Option[String], // 1 if None
          Target: String,               // Npc with area
      ) extends Types

      case class DruidKill(
          Target: String // Ai, Keyword(?)
      ) extends Types

      case class DruidScripted(Target: String) extends Types

      case class GuildKill(
          Target: String // Ai, Keyword(?)
      ) extends Types

      case class GuildGiveItem(
          ItemName: Option[String],    // Item
          ItemKeyword: Option[String], // Keyword
          Target: String,              // Npc
      ) extends Types

      case class GiveGift(
          MinFavorReceived: Option[String], // float
          MaxFavorReceived: Option[String], // float
      ) extends Types

      case class UseItem(
          ItemName: Option[String], // Item
          Target: Option[String],   // Keyword?
      ) extends Types

      case class UseRecipe(
          Skill: Option[String], // Skill
          Target: Option[String],// Recipe
      ) extends Types

      case class UseAbility(
          Target: String // Ability
      ) extends Types

      case class UseAbilityOnTargets(
          Target: String,        // Ai
          AbilityKeyword: String,// refs a group of abilities via Keyword?
      ) extends Types

      case class Have(
          ItemName: Option[String], // item
          Target: Option[String],   // keyword?
      ) extends Types

      case class Special(Target: String) extends Types

      case class UniqueSpecial(Target: String) extends Types

      case class InteractionFlag(
          Target: String // interaction flag
      ) extends Types

      case class MultipleInteractionFlags(
          InteractionFlags: List[String] // interaction flag
      ) extends Types

      case class MeetRequirements() extends Types // Requirements is always Some

      case class CompleteQuest(
          Target: String // Quest
      ) extends Types

      case class SayInChat(
          Target: String // chat message
      ) extends Types

      case class TipPlayer(
          MinAmount: String // Int
      ) extends Types

      case class CollectProcessed(
          ItemName: Option[Content.Ref[Item]],
          Target: Option[String], // keyword?
      ) extends Processed[Collect]

      case class ScriptedProcessed() extends Processed[Scripted]

      case class ScriptedReceiveItemProcessed(
          Item: Content.Ref[Item],
          Target: Info.Ref[Npc], // Npc with area
      ) extends Processed[ScriptedReceiveItem]

      case class ScriptAtomicIntProcessed(Target: String)
          extends Processed[ScriptAtomicInt]

      given Decoder[Kill] = new Decoder[Kill] {
        final def apply(c: HCursor): Decoder.Result[Kill] = {
          c.get[Option[String]]("AbilityKeyword")
            .flatMap(abilityKw =>
              c.downField("Target")
                .focus
                .map(_.toOptions2)
                .toRight(DecodingFailure("Failed to decode Target", c.history))
                .flatMap((target, area) =>
                  target.toRight(
                    DecodingFailure("Target is not string or array", c.history)
                  ) tupleRight area
                )
                .map((target, area) => Kill(abilityKw, target, area))
            )
        }
      }

      type KillTarget = Info.Ref[Ai] | String

      case class KillProcessed(
          AbilityKeyword: Option[String], // Keyword
          Target: KillTarget,             // Ai, Keyword(?), "*", "Area:${Area}"
          area: Option[Info.Ref[Area]],
      ) extends Processed[Kill]

      case class KillEliteProcessed(
          Target: String // Enemy keyword or "*"
      ) extends Processed[KillElite]

      case class HarvestProcessed(
          ItemName: Option[Content.Ref[Item]], // Item
          Target: Option[String],              // Item? Keyword?
      ) extends Processed[Harvest]

      case class LootProcessed(
          ItemName: Option[Content.Ref[Item]], // Item
          Target: Option[String],              // Keyword
          MonsterTypeTag: Option[String],      // Enemy keyword?
      ) extends Processed[Loot]

      case class BeAttackedProcessed(
          AnatomyType: Option[String],
          Target: Option[Info.Ref[Ai]],
      ) extends Processed[BeAttacked]

      case class BuryProcessed(
          AnatomyType: Option[String],
          Target: Option[Info.Ref[Ai]],
      ) extends Processed[Bury]

      case class DeliverProcessed(
          ItemName: Content.Ref[Item],
          NumToDeliver: Int, // 1 if None
          Target: Info.Ref[Npc],
      ) extends Processed[Deliver]

      case class DruidKillProcessed(
          Target: String // Enemy Keyword
      ) extends Processed[DruidKill]

      case class DruidScriptedProcessed(Target: String)
          extends Processed[DruidScripted]

      case class GuildKillProcessed(
          Target: String // Enemy Keyword
      ) extends Processed[GuildKill]

      case class GuildGiveItemProcessed(
          ItemName: Option[Content.Ref[Item]], // Item
          ItemKeyword: Option[String],         // Keyword
          Target: Info.Ref[Npc],               // Npc
      ) extends Processed[GuildGiveItem]

      case class GiveGiftProcessed(
          MinFavorReceived: Option[Float],
          MaxFavorReceived: Option[Float],
      ) extends Processed[GiveGift]

      case class UseItemProcessed(
          ItemName: Option[Content.Ref[Item]],
          Target: Option[String], // Keyword?
      ) extends Processed[UseItem]

      case class UseRecipeProcessed(
          Skill: Option[Info.Ref[Skill]],
          Target: Option[Content.Ref[Recipe]],
      ) extends Processed[UseRecipe]

      case class UseAbilityProcessed(
          Target: Content.Ref[Ability]
      ) extends Processed[UseAbility]

      case class UseAbilityOnTargetsProcessed(
          Target: Info.Ref[Ai],
          AbilityKeyword: String, // refs a group of abilities via Keyword?
      ) extends Processed[UseAbilityOnTargets]

      case class HaveProcessed(
          ItemName: Option[Content.Ref[Item]],
          Target: Option[String], // keyword?
      ) extends Processed[Have]

      case class SpecialProcessed(Target: String) extends Processed[Special]

      case class UniqueSpecialProcessed(Target: String)
          extends Processed[UniqueSpecial]

      case class InteractionFlagProcessed(
          Target: String // interaction flag
      ) extends Processed[InteractionFlag]

      case class MultipleInteractionFlagsProcessed(
          InteractionFlags: List[String] // interaction flag
      ) extends Processed[MultipleInteractionFlags]

      case class MeetRequirementsProcessed() extends Processed[MeetRequirements] // Requirements is always Some

      case class CompleteQuestProcessed(
          Target: Content.Ref[Quest]
      ) extends Processed[CompleteQuest]

      case class SayInChatProcessed(
          Target: String // chat message
      ) extends Processed[SayInChat]

      case class TipPlayerProcessed(
          MinAmount: Int
      ) extends Processed[TipPlayer]
    }

    given Decoder[Objective] = new Decoder[Objective] {
      final def apply(c: HCursor): Decoder.Result[Objective] = {
        def decodePatched(cPatched: HCursor): Decoder.Result[Objective] =
          (
            cPatched.get[String]("Description"),
            cPatched.get[Option[Int]]("Number"),
            cPatched.get[Option[Int]]("GroupId"),
            cPatched.get[Option[Boolean]](
              "IsHiddenUntilEarlierObjectivesComplete"
            ),
            cPatched.get[Option[List[Requirement]]]("Requirements"),
            cPatched.get[String]("Type") flatMap {
              case "Collect"  => Decoder[Types.Collect].apply(cPatched)
              case "Scripted" => Decoder[Types.Scripted].apply(cPatched)
              case "ScriptedReceiveItem" =>
                Decoder[Types.ScriptedReceiveItem].apply(cPatched)
              case "ScriptAtomicInt" =>
                Decoder[Types.ScriptAtomicInt].apply(cPatched)
              case "Kill"       => Decoder[Types.Kill].apply(cPatched)
              case "KillElite"  => Decoder[Types.KillElite].apply(cPatched)
              case "Harvest"    => Decoder[Types.Harvest].apply(cPatched)
              case "Loot"       => Decoder[Types.Loot].apply(cPatched)
              case "BeAttacked" => Decoder[Types.BeAttacked].apply(cPatched)
              case "Bury"       => Decoder[Types.Bury].apply(cPatched)
              case "Deliver"    => Decoder[Types.Deliver].apply(cPatched)
              case "DruidKill"  => Decoder[Types.DruidKill].apply(cPatched)
              case "DruidScripted" =>
                Decoder[Types.DruidScripted].apply(cPatched)
              case "GuildKill" => Decoder[Types.GuildKill].apply(cPatched)
              case "GuildGiveItem" =>
                Decoder[Types.GuildGiveItem].apply(cPatched)
              case "GiveGift"   => Decoder[Types.GiveGift].apply(cPatched)
              case "UseItem"    => Decoder[Types.UseItem].apply(cPatched)
              case "UseRecipe"  => Decoder[Types.UseRecipe].apply(cPatched)
              case "UseAbility" => Decoder[Types.UseAbility].apply(cPatched)
              case "UseAbilityOnTargets" =>
                Decoder[Types.UseAbilityOnTargets].apply(cPatched)
              case "Have"    => Decoder[Types.Have].apply(cPatched)
              case "Special" => Decoder[Types.Special].apply(cPatched)
              case "UniqueSpecial" =>
                Decoder[Types.UniqueSpecial].apply(cPatched)
              case "InteractionFlag" =>
                Decoder[Types.InteractionFlag].apply(cPatched)
              case "MultipleInteractionFlags" =>
                Decoder[Types.MultipleInteractionFlags].apply(cPatched)
              case "MeetRequirements" =>
                Decoder[Types.MeetRequirements].apply(cPatched)
              case "CompleteQuest" =>
                Decoder[Types.CompleteQuest].apply(cPatched)
              case "SayInChat" => Decoder[Types.SayInChat].apply(cPatched)
              case "TipPlayer" => Decoder[Types.TipPlayer].apply(cPatched)
              case s =>
                DecodingFailure(
                  s"Objective type not found: $s",
                  cPatched.history,
                ).asLeft
            },
          ) mapN Objective.apply

        c.downField("Requirements")
          .success
          .fold( // No Requirements field present, so decode original cursor
            decodePatched(c)
          )(cReq => // If Requirements is present, convert it to an array if needed
            cReq.wrapInArray.up.success.fold(
              DecodingFailure(
                "Failed to patch Objective.Requirements field",
                cReq.history,
              ).asLeft
            )(decodePatched)
          )
      }
    }
  }

  case class ObjectiveProcessed(
      Description: String,
      Number: Option[Int],
      GroupId: Option[Int],
      IsHiddenUntilEarlierObjectivesComplete: Option[Boolean],
      Requirements: Option[List[Processed[Requirement]]],
      Type: Processed[Objective.Types],
  ) extends Processed[Objective]

  // case class Requirement(
  //     AllowedRace: Option[String],     // with Race
  //     Appearance: Option[String],      // with Appearance
  //     AreaEvent: Option[String],       // with AreaEventOff/On
  //     DisallowedRace: Option[String],  // with Race
  //     HangOut: Option[String],         // with HangOutCompleted
  //     InteractionFlag: Option[String], // with InteractionFlagSet
  //     Keyword: Option[String],         // with HasEffectKeyword, refs Keyword
  //     skillLevel: Option[Int],         // skill level, with MinSkillLevel
  //     favorLevel: Option[String],      // favor level, with MinFavorLevel
  //     List: Option[List[Requirement]], // with Or
  //     MaxHour: Option[Int],            // with TimeOfDay
  //     MinHour: Option[Int],            // with TimeOfDay
  //     MoonPhase: Option[String],       // with MoonPhase
  //     Npc: Option[String],             // refs npc with area, with MinFavorLevel
  //     Quest: Option[String],           // refs quest, with QuestCompleted
  //     Rule: Option[String],            // with RuntimeBehaviorRuleSet
  //     Shape: Option[String],           // with GeneralShape
  //     Skill: Option[
  //       String
  //     ], // refs skill, with MinSkillLevel or ActiveCombatSkill
  //     T: Requirement.Type
  // )

  sealed trait Requirement // derives JsonTaggedAdt.PureEncoder, JsonTaggedAdt.PureDecoder {
  object Requirement {
    case class MinFavor(
        MinFavor: Int, // favor amount
        Npc: String,
    ) extends Requirement
    case class MinFavorProcessed(minFavor: Int, npc: Info.Ref[Npc])
        extends Processed[MinFavor]

    case class MinFavorLevel(
        Level: String, // favor level
        Npc: String,
    ) extends Requirement
    case class MinFavorLevelProcessed(favorLevel: String, npc: Info.Ref[Npc])
        extends Processed[MinFavorLevel]

    case class MinSkillLevel(
        Level: Int, // skill level
        Skill: String,
    ) extends Requirement
    case class MinSkillLevelProcessed(level: Int, skill: Info.Ref[Skill])
        extends Processed[MinSkillLevel]

    case class ActiveCombatSkill(Skill: String) extends Requirement
    case class ActiveCombatSkillProcessed(skill: Info.Ref[Skill])
        extends Processed[ActiveCombatSkill]

    case class EquipmentSlotEmpty(Slot: String) extends Requirement
    case class EquipmentSlotEmptyProcessed(slot: String)
        extends Processed[EquipmentSlotEmpty]

    case class InteractionFlagSet(InteractionFlag: String) extends Requirement
    case class InteractionFlagSetProcessed(interactionFlag: String)
        extends Processed[InteractionFlagSet]

    case class InteractionFlagUnset(InteractionFlag: String) extends Requirement
    case class InteractionFlagUnsetProcessed(interactionFlag: String)
        extends Processed[InteractionFlagUnset]

    case class QuestCompleted(Quest: String) extends Requirement
    case class QuestCompletedProcessed(quest: Content.Ref[Quest])
        extends Processed[QuestCompleted]

    case class QuestCompletedRecently(Quest: String) extends Requirement
    case class QuestCompletedRecentlyProcessed(quest: Content.Ref[Quest])
        extends Processed[QuestCompletedRecently]

    case class GuildQuestCompleted(Quest: String) extends Requirement
    case class GuildQuestCompletedProcessed(quest: Content.Ref[Quest])
        extends Processed[GuildQuestCompleted]

    case class AreaEventOff(AreaEvent: String) extends Requirement
    case class AreaEventOffProcessed(AreaEvent: String)
        extends Processed[AreaEventOff]

    case class AreaEventOn(AreaEvent: String) extends Requirement
    case class AreaEventOnProcessed(AreaEvent: String)
        extends Processed[AreaEventOn]

    case class IsWarden()          extends Requirement
    case class IsWardenProcessed() extends Processed[IsWarden]

    case class IsLongtimeAnimal()          extends Requirement
    case class IsLongtimeAnimalProcessed() extends Processed[IsLongtimeAnimal]

    case class GeneralShape(Shape: String) extends Requirement
    case class GeneralShapeProcessed(Shape: String)
        extends Processed[GeneralShape]

    case class MoonPhase(MoonPhase: String) extends Requirement
    case class MoonPhaseProcessed(MoonPhase: String) extends Processed[MoonPhase]

    case class RuntimeBehaviorRuleSet(Rule: String) extends Requirement
    case class RuntimeBehaviorRuleSetProcessed(Rule: String)
        extends Processed[RuntimeBehaviorRuleSet]

    case class HangOutCompleted(HangOut: String) extends Requirement
    case class HangOutCompletedProcessed(HangOut: String)
        extends Processed[HangOutCompleted]

    case class Race(AllowedRace: Option[String], DisallowedRace: Option[String])
        extends Requirement
    case class RaceProcessed(
        AllowedRace: Option[String],
        DisallowedRace: Option[String],
    ) extends Processed[Race]

    case class Or(List: List[Requirement]) extends Requirement
    case class OrProcessed(requirements: List[Processed[Requirement]])
        extends Processed[Or]

    case class HasEffectKeyword(Keyword: String) extends Requirement
    case class HasEffectKeywordProcessed(Keyword: String)
        extends Processed[HasEffectKeyword]

    case class Appearance(Appearance: String) extends Requirement
    case class AppearanceProcessed(Appearance: String)
        extends Processed[Appearance]

    case class TimeOfDay(MaxHour: Int, MinHour: Int) extends Requirement
    case class TimeOfDayProcessed(MaxHour: Int, MinHour: Int)
        extends Processed[TimeOfDay]

    case class PetCount(MaxCount: Int, MinCount: Int, PetTypeTag: String)
        extends Requirement
    case class PetCountProcessed(
        MaxCount: Int,
        MinCount: Int,
        PetTypeTag: String,
    ) extends Processed[PetCount]

    case class EntityPhysicalState(AllowedStates: List[String])
        extends Requirement
    case class EntityPhysicalStateProcessed(AllowedStates: List[String])
        extends Processed[EntityPhysicalState]

    case class ScriptAtomicMatches(AtomicVar: String, Value: String)
        extends Requirement
    case class ScriptAtomicMatchesProcessed(AtomicVar: String, Value: String)
        extends Processed[ScriptAtomicMatches]

    given Decoder[Requirement] = new Decoder[Requirement] {
      final def apply(c: HCursor): Decoder.Result[Requirement] =
        c.get[String]("T").flatMap {
          case "MinFavor"             => Decoder[MinFavor].apply(c)
          case "MinFavorLevel"        => Decoder[MinFavorLevel].apply(c)
          case "MinSkillLevel"        => Decoder[MinSkillLevel].apply(c)
          case "ActiveCombatSkill"    => Decoder[ActiveCombatSkill].apply(c)
          case "EquipmentSlotEmpty"   => Decoder[EquipmentSlotEmpty].apply(c)
          case "InteractionFlagSet"   => Decoder[InteractionFlagSet].apply(c)
          case "InteractionFlagUnset" => Decoder[InteractionFlagUnset].apply(c)
          case "QuestCompleted"       => Decoder[QuestCompleted].apply(c)
          case "QuestCompletedRecently" =>
            Decoder[QuestCompletedRecently].apply(c)
          case "GuildQuestCompleted" => Decoder[GuildQuestCompleted].apply(c)
          case "AreaEventOff"        => Decoder[AreaEventOff].apply(c)
          case "AreaEventOn"         => Decoder[AreaEventOn].apply(c)
          case "IsWarden"            => Decoder[IsWarden].apply(c)
          case "IsLongtimeAnimal"    => Decoder[IsLongtimeAnimal].apply(c)
          case "GeneralShape"        => Decoder[GeneralShape].apply(c)
          case "MoonPhase"           => Decoder[MoonPhase].apply(c)
          case "RuntimeBehaviorRuleSet" =>
            Decoder[RuntimeBehaviorRuleSet].apply(c)
          case "HangOutCompleted"    => Decoder[HangOutCompleted].apply(c)
          case "Race"                => Decoder[Race].apply(c)
          case "Or"                  => Decoder[Or].apply(c)
          case "HasEffectKeyword"    => Decoder[HasEffectKeyword].apply(c)
          case "Appearance"          => Decoder[Appearance].apply(c)
          case "TimeOfDay"           => Decoder[TimeOfDay].apply(c)
          case "PetCount"            => Decoder[PetCount].apply(c)
          case "EntityPhysicalState" => Decoder[EntityPhysicalState].apply(c)
          case "ScriptAtomicMatches" => Decoder[ScriptAtomicMatches].apply(c)
          case s =>
            DecodingFailure(s"Requirement type not found: $s", c.history).asLeft
        }
    }
  }

  enum Reward {
    case SkillXp(Skill: String, Xp: Int)
    case CombatXp(Xp: Int)
    case GuildXp(Xp: Int)
    case GuildCredits(Credits: Int)
    case Currency(Amount: Int, Currency: Reward.CurrencyType)
    case WorkOrderCurrency(Amount: Int, Currency: Reward.CurrencyType)
    case Recipe(Recipe: String)
    case Ability(Ability: String)
  }

  object Reward {
    enum CurrencyType
        derives JsonTaggedAdt.PureEncoder,
          JsonTaggedAdt.PureDecoder {
      case Gold, WardenPoints, LiveEventCredits
    }

    case class SkillXpProcessed(skill: Info.Ref[Skill], Xp: Int)
        extends Processed[SkillXp]
    case class CombatXpProcessed(xp: Int) extends Processed[CombatXp]
    case class GuildXpProcessed(xp: Int)  extends Processed[GuildXp]
    case class GuildCreditsProcessed(credits: Int)
        extends Processed[GuildCredits]
    case class CurrencyProcessed(amount: Int, currency: CurrencyType)
        extends Processed[Currency]
    case class WorkOrderCurrencyProcessed(
        amount: Int,
        currency: CurrencyType,
    ) extends Processed[WorkOrderCurrency]
    case class RecipeProcessed(recipe: Content.Ref[gorgonartisan.Recipe])
        extends Processed[Recipe]
    case class AbilityProcessed(ability: Content.Ref[gorgonartisan.Ability])
        extends Processed[Ability]

    given Decoder[Reward] = new Decoder[Reward] {
      final def apply(c: HCursor): Decoder.Result[Reward] =
        c.get[String]("T").flatMap {
          case "SkillXp"           => Decoder[SkillXp].apply(c)
          case "CombatXp"          => Decoder[CombatXp].apply(c)
          case "GuildXp"           => Decoder[GuildXp].apply(c)
          case "GuildCredits"      => Decoder[GuildCredits].apply(c)
          case "Currency"          => Decoder[Currency].apply(c)
          case "WorkOrderCurrency" => Decoder[WorkOrderCurrency].apply(c)
          case "Recipe"            => Decoder[Recipe].apply(c)
          case "Ability"           => Decoder[Ability].apply(c)
          case s =>
            DecodingFailure(s"Reward type not found: $s", c.history).asLeft
        }
    }
  }

  given Decoder[Quest] = new Decoder[Quest] {
    final def apply(c: HCursor): Decoder.Result[Quest] = for {
      Description             <- c.get[String]("Description")
      DisplayedLocation       <- c.get[Option[String]]("DisplayedLocation")
      FavorNpc                <- c.get[Option[String]]("FavorNpc")
      FollowUpQuests          <- c.get[Option[List[String]]]("FollowUpQuests")
      GroupingName            <- c.get[Option[String]]("GroupingName")
      InternalName            <- c.get[String]("InternalName")
      IsAutoPreface           <- c.get[Option[Boolean]]("IsAutoPreface")
      IsAutoWrapUp            <- c.get[Option[Boolean]]("IsAutoWrapUp")
      IsCancellable           <- c.get[Option[Boolean]]("IsCancellable")
      Keywords                <- c.get[Option[List[String]]]("Keywords")
      Level                   <- c.get[Option[Int]]("Level")
      MidwayText              <- c.get[Option[String]]("MidwayText")
      Name                    <- c.get[String]("Name")
      NumExpectedParticipants <- c.get[Option[Int]]("NumExpectedParticipants")
      Objectives              <- c.get[List[Quest.Objective]]("Objectives")
      PrefaceText             <- c.get[Option[String]]("PrefaceText")
      PreGiveEffects          <- c.get[Option[List[String]]]("PreGiveEffects")
      PreGiveRecipes          <- c.get[Option[List[String]]]("PreGiveRecipes")
      // If Requirements is an object, wrap it in an array
      Requirements <- c
        .downField("Requirements")
        .flattenArray
        .wrapInArray
        .as[Option[List[Quest.Requirement]]]
      RequirementsToSustain <- c
        .downField("RequirementsToSustain")
        .wrapInArray
        .as[Option[List[Quest.Requirement]]]
      ReuseTime_Minutes <- c.get[Option[Int]]("ReuseTime_Minutes")
      ReuseTime_Hours   <- c.get[Option[Int]]("ReuseTime_Hours")
      Reward_Favor      <- c.get[Option[Int]]("Reward_Favor")
      Rewards           <- c.get[Option[List[Quest.Reward]]]("Rewards")
      Rewards_Effects   <- c.get[Option[List[String]]]("Rewards_Effects")
      Rewards_Items     <- c.get[Option[List[ItemStack]]]("Rewards_Items")
      Rewards_NamedLootProfile <- c.get[Option[String]](
        "Rewards_NamedLootProfile"
      )
      SuccessText <- c.get[Option[String]]("SuccessText")
      TSysLevel   <- c.get[Option[Int]]("TSysLevel")
      Version     <- c.get[Int]("Version")
    } yield Quest(
      Description,
      DisplayedLocation,
      FavorNpc,
      FollowUpQuests,
      GroupingName,
      InternalName,
      IsAutoPreface,
      IsAutoWrapUp,
      IsCancellable,
      Keywords,
      Level,
      MidwayText,
      Name,
      NumExpectedParticipants,
      Objectives,
      PrefaceText,
      PreGiveEffects,
      PreGiveRecipes,
      Requirements,
      RequirementsToSustain,
      ReuseTime_Minutes,
      ReuseTime_Hours,
      Reward_Favor,
      Rewards,
      Rewards_Effects,
      Rewards_Items,
      Rewards_NamedLootProfile,
      SuccessText,
      TSysLevel,
      Version,
    )
  }
}

case class QuestProcessed(
    Description: String,
    DisplayedLocation: Option[Info.Ref[Area]],
    FavorNpc: Option[Info.Ref[Npc]],
    FollowUpQuests: Option[List[Content.Ref[Quest]]],
    InternalName: String,
    IsCancellable: Option[Boolean],
    Keywords: Option[List[String]], // refs keyword
    MidwayText: Option[String],
    Name: String,
    Objectives: List[Processed[Quest.Objective]],
    PrefaceText: Option[String],
    PreGiveRecipes: Option[List[Content.Ref[Recipe]]],
    Requirements: Option[List[Processed[Quest.Requirement]]],
    RequirementsToSustain: Option[List[Processed[Quest.Requirement]]],
    ReuseTime_Minutes: Option[Int],
    ReuseTime_Hours: Option[Int],
    Reward_Favor: Option[Int],
    Rewards: Option[List[Quest.Reward]],
    Rewards_Effects: Option[List[String]], // refs effect?
    Rewards_Items: Option[List[ItemStack]],
    Rewards_NamedLootProfile: Option[String],
    SuccessText: Option[String],
    TSysLevel: Option[Int],
    Version: Int,
) extends Processed[Quest]
