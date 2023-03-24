package gorgonartisan

import scala.annotation.experimental
import scala.compiletime.erasedValue
import scala.compiletime.summonInline
import scala.deriving.Mirror

import gorgonartisan.Quest.*

import cats.implicits.*

trait Processed[+A] // extends Product

trait ProcessData[A] {
  extension (a: A) def process: Processed[A]
}

// object Processed {
// @experimental
object ProcessData {
  inline def summonAll[T <: Tuple]: List[ProcessData[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[ProcessData[t]] :: summonAll[ts]
    }

  inline def pdProduct[FROM <: Product, TO <: Processed[FROM]](
      inline mTo: Mirror.ProductOf[TO]
      // inline mFrom: Mirror.ProductOf[FROM],
  ) =
    // mTo match {
    //   case mTo: Mirror.ProductOf[TO] =>
    //     mFrom match {
    //       case mFrom: Mirror.ProductOf[FROM] {
    //             type MirroredElemTypes = mTo.MirroredElemTypes
    //           } =>
    new ProcessData[FROM] {
      extension (from: FROM) {
        def process =
          mTo.fromProduct(from) // (using
          // mFrom.copy {
          //   type MirroredElemTypes = mTo.MirroredElemTypes
          // }
          // )
      }
    }
  //     }
  // }

  // import shapeless3.deriving.*

  // given gen[A, P <: Processed[A]](using
  //     inst: K0.ProductInstances[ProcessData, A]
  // ): ProcessData[A] with {
  //   extension (a: A) {
  //     def process =
  //       inst.map(a)(
  //         [t] => (pd: ProcessData[t], t0: t) => pd.process(t0)
  //       )
  //   }
  // }

  // given pdProduct[A <: Product, P <: Processed[A] with Product](using
  //     // mp: Mirror.ProductOf[P],
  //     // ma: Mirror.ProductOf[A] { type MirroredElemTypes = mp.MirroredElemTypes },
  //     aGen: K0.ProductGeneric[A],
  //     pGen: K0.ProductGeneric[P],
  //     // pInst: K0.CoproductInstances[Processed, P],
  //     // pGen: K0.ProductGeneric[Processed[A]],
  //     // aLabelling: Labelling[A],
  // ): ProcessData[A] with {
  //   // val p = pInst.summonOnly[Processed, A]
  //   extension (a: A) {
  //     def process = pGen.fromRepr(aGen.toRepr(a).asInstanceOf) // mp.fromProductTyped(a) // (using ma)
  //     // Tuple.fromProductTyped(a).map[Processed]([t] => (p: Processed[t]) => p) // pGen.fromRepr(aGen.toRepr(a).map(_.lower).asInstanceOf)
  //     // inst.map(a)(
  //     //   [t] => (pd: ProcessData[t], t0: t) => pd.process(t0)
  //     // )
  //   }
  // }

  inline def pdSum[A](
      inline m: Mirror.SumOf[A]
      // gen: K0.CoproductGeneric[A],
      // inst: K0.CoproductInstances[ProcessData, A]
      // ma: Mirror.SumOf[A] { type MirroredElemTypes = mp.MirroredElemTypes },
      // aGen: K0.ProductGeneric[A],
      // pGen: K0.ProductGeneric[Processed[A]],
      // aLabelling: Labelling[A],
  ): ProcessData[A] = {
    val elemInstances = summonAll[m.MirroredElemTypes]
    new ProcessData[A] {
      extension (a: A) {
        def process = {
          // given ProcessData[A] =
          elemInstances(m.ordinal(a)).asInstanceOf[ProcessData[A]].process(a) //
          // a.process
        } // (using ma)
        // def process =
        // inst.fold(a)([t <: A] => (pdt: ProcessData[t], t0: t) => pdt.process(t0)) // summon[K2.CoproductInstances]
        // Tuple.fromProductTyped(a).map[Processed]([t] => (p: Processed[t]) => p) // pGen.fromRepr(aGen.toRepr(a).map(_.lower).asInstanceOf)
        // inst.map(a)(
        //   [t] => (pd: ProcessData[t], t0: t) => pd.process(t0)
        // )
      }
    }
  }

  // inline def derived[A, P <: Processed[A]](using
  //     gen: K0.Generic[A]
  // ): ProcessData[A] = gen.derive(pdProduct, pdSum)

  inline given derived[FROM <: Product, TO <: Processed[FROM]](using
      inline mFrom: Mirror.Of[FROM],
      inline mTo: Mirror.Of[TO],
  ): ProcessData[FROM] =
    inline mFrom match {
      case mFrom: Mirror.SumOf[FROM] => pdSum[FROM](mFrom)
      case mFrom: Mirror.ProductOf[FROM] =>
        mTo match {
          case mTo: Mirror.ProductOf[TO] =>
            pdProduct[FROM, TO](mTo)
        }
    }

// Info
  given ProcessData[Source] with {
    extension (source: Source) {
      def process = SourceProcessed(
        source.Type,
        source.Npc map Info.byKey[Npc],
        source.SkillTypeId map Info.byKey[Skill],
        source.ItemTypeId map Content.byId[Item],
      )
    }
  }

// Content
  given ProcessData[Content] with {
    extension (content: Content) {
      def process = content match {
        case item: Item     => summon[ProcessData[Item]].process(item)
        case recipe: Recipe => summon[ProcessData[Recipe]].process(recipe)
        case quest: Quest   => summon[ProcessData[Quest]].process(quest)
      }
    }
  }

  given ProcessData[ItemStack] with {
    extension (itemStack: ItemStack) {
      def process = ItemStackProcessed(
        itemStack.ChanceToConsume,
        itemStack.Desc,
        itemStack.ItemCode.map(Content.byId[Item]),
        itemStack.ItemKeys,
        itemStack.StackSize,
      )
    }
  }

  given ProcessData[Recipe] with {
    extension (recipe: Recipe) {
      def process = RecipeProcessed(
        recipe.ActionLabel,
        recipe.Description,
        recipe.IconId,
        recipe.Ingredients.map(_.process),
        recipe.InternalName,
        recipe.ItemMenuCategoryLevel,
        recipe.ItemMenuKeywordReq,
        recipe.ItemMenuLabel,
        recipe.Keywords,
        recipe.LoopParticle,
        recipe.Name,
        recipe.Particle,
        recipe.PrereqRecipe.map(Content.byIname),
        recipe.ProtoResultItems.map(_.map(_.process)),
        recipe.ResetTimeInSeconds,
        recipe.ResultEffects,
        recipe.ResultItems.map(_.process),
        Info.byKey[Skill](recipe.RewardSkill),
        recipe.RewardSkillXp,
        recipe.RewardSkillXpDropOffLevel,
        recipe.RewardSkillXpDropOffPct,
        recipe.RewardSkillXpDropOffRate,
        recipe.RewardSkillXpFirstTime,
        Info.byKey[Skill](recipe.Skill),
        recipe.SkillLevelReq,
        recipe.SharesResetTimerWith.map(Content.byIname),
        recipe.SortSkill.map(Info.byKey[Skill]),
        recipe.UsageAnimation,
        recipe.UsageAnimationEnd,
        recipe.UsageDelay,
        recipe.UsageDelayMessage,
      )
    }
  }

  given ProcessData[Item] with {
    extension (item: Item) {
      def process = ItemProcessed(
        item.Behaviors,
        item.BestowQuest map Content.byIname[Quest],
        item.BestowRecipes.map(_ map Content.byIname[Recipe]),
        item.BestowTitle,
        item.CraftPoints,
        item.CraftingTargetLevel,
        item.Description,
        item.DroppedAppearance,
        item.EffectDescs,
        item.EquipAppearance,
        item.EquipSlot,
        item.FoodDesc,
        item.IconId,
        item.InternalName,
        item.Keywords,
        item.Lint_VendorNpc map Info.byKey[Npc],
        item.MacGuffinQuestName map Content.byIname[Quest],
        item.MaxCarryable,
        item.MaxStackSize,
        item.Name,
        item.NumUses,
        item.SkillReqs.map(_.map((skill, v) => (Info.byKey[Skill](skill), v))),
        item.StockDye,
        item.TSysProfile,
        item.Value,
      )
    }
  }
// Quest

  given ProcessData[Quest] with {
    extension (quest: Quest) {
      def process = QuestProcessed(
        quest.Description,
        quest.DisplayedLocation map Info.byKey[Area],
        quest.FavorNpc map Info.byNpcName,
        quest.FollowUpQuests.map(_ map Content.byIname[Quest]),
        quest.InternalName,
        quest.IsCancellable,
        quest.Keywords,
        quest.MidwayText,
        quest.Name,
        quest.Objectives.map(_.process),
        quest.PrefaceText,
        quest.PreGiveRecipes.map(_ map Content.byIname[Recipe]),
        quest.Requirements.map(_.map(_.process)),
        quest.RequirementsToSustain.map(_.map(_.process)),
        quest.ReuseTime_Minutes,
        quest.ReuseTime_Hours,
        quest.Reward_Favor,
        quest.Rewards,
        quest.Rewards_Effects,
        quest.Rewards_Items,
        quest.Rewards_NamedLootProfile,
        quest.SuccessText,
        quest.TSysLevel,
        quest.Version,
      )
    }
  }
// Reward
  given ProcessData[Reward] with {
    extension (reward: Reward) {
      def process = reward match {
        case r: Reward.SkillXp => summon[ProcessData[Reward.SkillXp]].process(r)
        case r: Reward.CombatXp =>
          summon[ProcessData[Reward.CombatXp]].process(r)
        case r: Reward.GuildXp => summon[ProcessData[Reward.GuildXp]].process(r)
        case r: Reward.GuildCredits =>
          summon[ProcessData[Reward.GuildCredits]].process(r)
        case r: Reward.Currency =>
          summon[ProcessData[Reward.Currency]].process(r)
        case r: Reward.WorkOrderCurrency =>
          summon[ProcessData[Reward.WorkOrderCurrency]].process(r)
        case r: Reward.Recipe  => summon[ProcessData[Reward.Recipe]].process(r)
        case r: Reward.Ability => summon[ProcessData[Reward.Ability]].process(r)
      }
    }
  }
  given ProcessData[Reward.SkillXp] with {
    extension (r: Reward.SkillXp) {
      def process = Reward.SkillXpProcessed(Info.byKey[Skill](r.Skill), r.Xp)
    }
  }
  given ProcessData[Reward.CombatXp] with {
    extension (r: Reward.CombatXp) {
      def process = Reward.CombatXpProcessed(r.Xp)
    }
  }
  given ProcessData[Reward.GuildXp] with {
    extension (r: Reward.GuildXp) {
      def process = Reward.GuildXpProcessed(r.Xp)
    }
  }
  given ProcessData[Reward.GuildCredits] with {
    extension (r: Reward.GuildCredits) {
      def process = Reward.GuildCreditsProcessed(r.Credits)
    }
  }
  given ProcessData[Reward.Currency] with {
    extension (r: Reward.Currency) {
      def process =
        Reward.CurrencyProcessed(r.Amount, r.Currency)
    }
  }
  given ProcessData[Reward.WorkOrderCurrency] with {
    extension (r: Reward.WorkOrderCurrency) {
      def process = Reward.WorkOrderCurrencyProcessed(
        r.Amount,
        r.Currency,
      )
    }
  }
  given pRewardRecipe: ProcessData[Reward.Recipe] =
    new ProcessData[Reward.Recipe] {
      extension (r: Reward.Recipe) {
        def process = Reward.RecipeProcessed(Content.byIname[Recipe](r.Recipe))
      }
    }
  given ProcessData[Reward.Ability] with {
    extension (r: Reward.Ability) {
      def process = Reward.AbilityProcessed(Content.byIname[Ability](r.Ability))
    }
  }

  given ProcessData[Requirement] with {
    extension (req: Requirement) {
      def process = req match {
        case r: Requirement.MinFavor =>
          summon[ProcessData[Requirement.MinFavor]].process(r)
        case r: Requirement.MinFavorLevel =>
          summon[ProcessData[Requirement.MinFavorLevel]].process(r)
        case r: Requirement.MinSkillLevel =>
          summon[ProcessData[Requirement.MinSkillLevel]].process(r)
        case r: Requirement.ActiveCombatSkill =>
          summon[ProcessData[Requirement.ActiveCombatSkill]].process(r)
        case r: Requirement.EquipmentSlotEmpty =>
          summon[ProcessData[Requirement.EquipmentSlotEmpty]].process(r)
        case r: Requirement.InteractionFlagSet =>
          summon[ProcessData[Requirement.InteractionFlagSet]].process(r)
        case r: Requirement.InteractionFlagUnset =>
          summon[ProcessData[Requirement.InteractionFlagUnset]].process(r)
        case r: Requirement.QuestCompleted =>
          summon[ProcessData[Requirement.QuestCompleted]].process(r)
        case r: Requirement.QuestCompletedRecently =>
          summon[ProcessData[Requirement.QuestCompletedRecently]].process(r)
        case r: Requirement.GuildQuestCompleted =>
          summon[ProcessData[Requirement.GuildQuestCompleted]].process(r)
        case r: Requirement.AreaEventOff =>
          summon[ProcessData[Requirement.AreaEventOff]].process(r)
        case r: Requirement.AreaEventOn =>
          summon[ProcessData[Requirement.AreaEventOn]].process(r)
        case r: Requirement.IsWarden =>
          summon[ProcessData[Requirement.IsWarden]].process(r)
        case r: Requirement.IsLongtimeAnimal =>
          summon[ProcessData[Requirement.IsLongtimeAnimal]].process(r)
        case r: Requirement.GeneralShape =>
          summon[ProcessData[Requirement.GeneralShape]].process(r)
        case r: Requirement.MoonPhase =>
          summon[ProcessData[Requirement.MoonPhase]].process(r)
        case r: Requirement.RuntimeBehaviorRuleSet =>
          summon[ProcessData[Requirement.RuntimeBehaviorRuleSet]].process(r)
        case r: Requirement.HangOutCompleted =>
          summon[ProcessData[Requirement.HangOutCompleted]].process(r)
        case r: Requirement.Race =>
          summon[ProcessData[Requirement.Race]].process(r)
        case r: Requirement.Or => summon[ProcessData[Requirement.Or]].process(r)
        case r: Requirement.HasEffectKeyword =>
          summon[ProcessData[Requirement.HasEffectKeyword]].process(r)
        case r: Requirement.Appearance =>
          summon[ProcessData[Requirement.Appearance]].process(r)
        case r: Requirement.TimeOfDay =>
          summon[ProcessData[Requirement.TimeOfDay]].process(r)
        case r: Requirement.PetCount =>
          summon[ProcessData[Requirement.PetCount]].process(r)
        case r: Requirement.EntityPhysicalState =>
          summon[ProcessData[Requirement.EntityPhysicalState]].process(r)
        case r: Requirement.ScriptAtomicMatches =>
          summon[ProcessData[Requirement.ScriptAtomicMatches]].process(r)
      }
    }
  }

  given ProcessData[Requirement.MinFavor] with {
    extension (r: Requirement.MinFavor) {
      def process =
        Requirement.MinFavorProcessed(r.MinFavor, Info.byNpcName(r.Npc))
    }
  }

  given ProcessData[Requirement.MinFavorLevel] with {
    extension (r: Requirement.MinFavorLevel) {
      def process =
        Requirement.MinFavorLevelProcessed(r.Level, Info.byNpcName(r.Npc))
    }
  }

  given ProcessData[Requirement.MinSkillLevel] with {
    extension (r: Requirement.MinSkillLevel) {
      def process =
        Requirement.MinSkillLevelProcessed(
          r.Level,
          Info.byKey[Skill](r.Skill),
        )
    }
  }

  given ProcessData[Requirement.ActiveCombatSkill] with {
    extension (r: Requirement.ActiveCombatSkill) {
      def process =
        Requirement.ActiveCombatSkillProcessed(Info.byKey[Skill](r.Skill))
    }
  }

  given ProcessData[Requirement.EquipmentSlotEmpty] = ProcessData.derived[
    Requirement.EquipmentSlotEmpty,
    Requirement.EquipmentSlotEmptyProcessed,
  ]

  given ProcessData[Requirement.InteractionFlagSet] = ProcessData.derived[
    Requirement.InteractionFlagSet,
    Requirement.InteractionFlagSetProcessed,
  ]

  given ProcessData[Requirement.InteractionFlagUnset] = ProcessData.derived[
    Requirement.InteractionFlagUnset,
    Requirement.InteractionFlagUnsetProcessed,
  ]

  given ProcessData[Requirement.QuestCompleted] with {
    extension (r: Requirement.QuestCompleted) {
      def process = Requirement.QuestCompletedProcessed(Content.byIname(r.Quest))
    }
  }

  given ProcessData[Requirement.QuestCompletedRecently] with {
    extension (r: Requirement.QuestCompletedRecently) {
      def process =
        Requirement.QuestCompletedRecentlyProcessed(Content.byIname(r.Quest))
    }
  }

  given ProcessData[Requirement.GuildQuestCompleted] with {
    extension (r: Requirement.GuildQuestCompleted) {
      def process =
        Requirement.GuildQuestCompletedProcessed(Content.byIname(r.Quest))
    }
  }

  given ProcessData[Requirement.AreaEventOff] = ProcessData.derived[
    Requirement.AreaEventOff,
    Requirement.AreaEventOffProcessed,
  ]

  given ProcessData[Requirement.AreaEventOn] = ProcessData.derived[
    Requirement.AreaEventOn,
    Requirement.AreaEventOnProcessed,
  ]

  given ProcessData[Requirement.IsWarden] = ProcessData.derived[
    Requirement.IsWarden,
    Requirement.IsWardenProcessed,
  ]

  given ProcessData[Requirement.IsLongtimeAnimal] = ProcessData.derived[
    Requirement.IsLongtimeAnimal,
    Requirement.IsLongtimeAnimalProcessed,
  ]

  given ProcessData[Requirement.GeneralShape] = ProcessData.derived[
    Requirement.GeneralShape,
    Requirement.GeneralShapeProcessed,
  ]

  given ProcessData[Requirement.MoonPhase] = ProcessData.derived[
    Requirement.MoonPhase,
    Requirement.MoonPhaseProcessed,
  ]

  given ProcessData[Requirement.RuntimeBehaviorRuleSet] = ProcessData.derived[
    Requirement.RuntimeBehaviorRuleSet,
    Requirement.RuntimeBehaviorRuleSetProcessed,
  ]

  given ProcessData[Requirement.HangOutCompleted] = ProcessData.derived[
    Requirement.HangOutCompleted,
    Requirement.HangOutCompletedProcessed,
  ]

  given ProcessData[Requirement.Race] = ProcessData.derived[
    Requirement.Race,
    Requirement.RaceProcessed,
  ]

  given ProcessData[Requirement.Or] with {
    extension (r: Requirement.Or) {
      def process: Requirement.OrProcessed =
        Requirement.OrProcessed(r.List.map(_.process))
    }
  }

  given ProcessData[Requirement.HasEffectKeyword] = ProcessData.derived[
    Requirement.HasEffectKeyword,
    Requirement.HasEffectKeywordProcessed,
  ]

  given ProcessData[Requirement.Appearance] = ProcessData.derived[
    Requirement.Appearance,
    Requirement.AppearanceProcessed,
  ]

  given ProcessData[Requirement.TimeOfDay] = ProcessData.derived[
    Requirement.TimeOfDay,
    Requirement.TimeOfDayProcessed,
  ]

  given ProcessData[Requirement.PetCount] = ProcessData.derived[
    Requirement.PetCount,
    Requirement.PetCountProcessed,
  ]

  given ProcessData[Requirement.EntityPhysicalState] = ProcessData.derived[
    Requirement.EntityPhysicalState,
    Requirement.EntityPhysicalStateProcessed,
  ]

  given ProcessData[Requirement.ScriptAtomicMatches] = ProcessData.derived[
    Requirement.ScriptAtomicMatches,
    Requirement.ScriptAtomicMatchesProcessed,
  ]

  given ProcessData[Objective] with {
    extension (t: Objective) {
      def process = ObjectiveProcessed(
        t.Description,
        t.Number,
        t.GroupId,
        t.IsHiddenUntilEarlierObjectivesComplete,
        t.Requirements.map(_.map(_.process)),
        t.Type.process,
      )
    }
  }

// Objective Types
  given ProcessData[Objective.Types] with {
    extension (ot: Objective.Types) {
      def process = ot match {
        case t: Objective.Types.Collect =>
          summon[ProcessData[Objective.Types.Collect]].process(t)
        case t: Objective.Types.Scripted =>
          summon[ProcessData[Objective.Types.Scripted]].process(t)
        case t: Objective.Types.ScriptedReceiveItem =>
          summon[ProcessData[Objective.Types.ScriptedReceiveItem]].process(t)
        case t: Objective.Types.ScriptAtomicInt =>
          summon[ProcessData[Objective.Types.ScriptAtomicInt]].process(t)
        case t: Objective.Types.Kill =>
          summon[ProcessData[Objective.Types.Kill]].process(t)
        case t: Objective.Types.KillElite =>
          summon[ProcessData[Objective.Types.KillElite]].process(t)
        case t: Objective.Types.Harvest =>
          summon[ProcessData[Objective.Types.Harvest]].process(t)
        case t: Objective.Types.Loot =>
          summon[ProcessData[Objective.Types.Loot]].process(t)
        case t: Objective.Types.BeAttacked =>
          summon[ProcessData[Objective.Types.BeAttacked]].process(t)
        case t: Objective.Types.Bury =>
          summon[ProcessData[Objective.Types.Bury]].process(t)
        case t: Objective.Types.Deliver =>
          summon[ProcessData[Objective.Types.Deliver]].process(t)
        case t: Objective.Types.DruidKill =>
          summon[ProcessData[Objective.Types.DruidKill]].process(t)
        case t: Objective.Types.DruidScripted =>
          summon[ProcessData[Objective.Types.DruidScripted]].process(t)
        case t: Objective.Types.GuildKill =>
          summon[ProcessData[Objective.Types.GuildKill]].process(t)
        case t: Objective.Types.GuildGiveItem =>
          summon[ProcessData[Objective.Types.GuildGiveItem]].process(t)
        case t: Objective.Types.GiveGift =>
          summon[ProcessData[Objective.Types.GiveGift]].process(t)
        case t: Objective.Types.UseItem =>
          summon[ProcessData[Objective.Types.UseItem]].process(t)
        case t: Objective.Types.UseRecipe =>
          summon[ProcessData[Objective.Types.UseRecipe]].process(t)
        case t: Objective.Types.UseAbility =>
          summon[ProcessData[Objective.Types.UseAbility]].process(t)
        case t: Objective.Types.UseAbilityOnTargets =>
          summon[ProcessData[Objective.Types.UseAbilityOnTargets]].process(t)
        case t: Objective.Types.Have =>
          summon[ProcessData[Objective.Types.Have]].process(t)
        case t: Objective.Types.Special =>
          summon[ProcessData[Objective.Types.Special]].process(t)
        case t: Objective.Types.UniqueSpecial =>
          summon[ProcessData[Objective.Types.UniqueSpecial]].process(t)
        case t: Objective.Types.InteractionFlag =>
          summon[ProcessData[Objective.Types.InteractionFlag]].process(t)
        case t: Objective.Types.MultipleInteractionFlags =>
          summon[ProcessData[Objective.Types.MultipleInteractionFlags]]
            .process(t)
        case t: Objective.Types.MeetRequirements =>
          summon[ProcessData[Objective.Types.MeetRequirements]].process(t)
        case t: Objective.Types.CompleteQuest =>
          summon[ProcessData[Objective.Types.CompleteQuest]].process(t)
        case t: Objective.Types.SayInChat =>
          summon[ProcessData[Objective.Types.SayInChat]].process(t)
        case t: Objective.Types.TipPlayer =>
          summon[ProcessData[Objective.Types.TipPlayer]].process(t)
      }
    }
  }

  given ProcessData[Objective.Types.Collect] with {
    extension (t: Objective.Types.Collect) {
      def process = Objective.Types.CollectProcessed(
        t.ItemName.map(Content.byIname[Item]),
        t.Target,
      )
    }
  }

  given ProcessData[Objective.Types.Scripted] with {
    extension (t: Objective.Types.Scripted) {
      def process = Objective.Types.ScriptedProcessed()
    }
  }

  given ProcessData[Objective.Types.ScriptedReceiveItem] with {
    extension (t: Objective.Types.ScriptedReceiveItem) {
      def process = Objective.Types.ScriptedReceiveItemProcessed(
        Content.byIname(t.Item),
        Info.byNpcName(t.Target),
      )
    }
  }

  given ProcessData[Objective.Types.ScriptAtomicInt] with {
    extension (t: Objective.Types.ScriptAtomicInt) {
      def process = Objective.Types.ScriptAtomicIntProcessed(
        t.Target: String
      )
    }
  }

  given ProcessData[Objective.Types.Kill] with {
    extension (t: Objective.Types.Kill) {
      def process = Objective.Types.KillProcessed(
        t.AbilityKeyword,
        t.Target match {
          case t
              if t == "*"
                || t.startsWith("AnatomyType")
                || t.startsWith("ExtraTag") =>
            t
          case t => Info.byKey[Ai](t)
        },
        t.area map Info.byKey[Area],
      )
    }
  }

  given ProcessData[Objective.Types.KillElite] with {
    extension (t: Objective.Types.KillElite) {
      def process = Objective.Types.KillEliteProcessed(
        t.Target
      )
    }
  }

  given ProcessData[Objective.Types.Harvest] with {
    extension (t: Objective.Types.Harvest) {
      def process = Objective.Types.HarvestProcessed(
        t.ItemName map Content.byIname[Item],
        t.Target,
      )
    }
  }

  given ProcessData[Objective.Types.Loot] with {
    extension (t: Objective.Types.Loot) {
      def process = Objective.Types.LootProcessed(
        t.ItemName map Content.byIname[Item],
        t.Target,
        t.MonsterTypeTag,
      )
    }
  }

  given ProcessData[Objective.Types.BeAttacked] with {
    extension (t: Objective.Types.BeAttacked) {
      def process = Objective.Types.BeAttackedProcessed(
        t.AnatomyType,
        t.Target map Info.byKey[Ai],
      )
    }
  }

  given ProcessData[Objective.Types.Bury] with {
    extension (t: Objective.Types.Bury) {
      def process = Objective.Types.BuryProcessed(
        t.AnatomyType,
        t.Target map Info.byKey[Ai],
      )
    }
  }

  given ProcessData[Objective.Types.Deliver] with {
    extension (t: Objective.Types.Deliver) {
      def process = Objective.Types.DeliverProcessed(
        Content.byIname[Item](t.ItemName),
        t.NumToDeliver.map(_.toInt).getOrElse(1),
        Info.byNpcName(t.Target),
      )
    }
  }

  given ProcessData[Objective.Types.DruidKill] with {
    extension (t: Objective.Types.DruidKill) {
      def process = Objective.Types.DruidKillProcessed(
        t.Target
      )
    }
  }

  given ProcessData[Objective.Types.DruidScripted] with {
    extension (t: Objective.Types.DruidScripted) {
      def process = Objective.Types.DruidScriptedProcessed(
        t.Target
      )
    }
  }

  given ProcessData[Objective.Types.GuildKill] with {
    extension (t: Objective.Types.GuildKill) {
      def process = Objective.Types.GuildKillProcessed(
        t.Target
      )
    }
  }

  given ProcessData[Objective.Types.GuildGiveItem] with {
    extension (t: Objective.Types.GuildGiveItem) {
      def process = Objective.Types.GuildGiveItemProcessed(
        t.ItemName map Content.byIname[Item],
        t.ItemKeyword: Option[String], // Keyword
        Info.byNpcName(t.Target),
      )
    }
  }

  given ProcessData[Objective.Types.GiveGift] with {
    extension (t: Objective.Types.GiveGift) {
      def process = Objective.Types.GiveGiftProcessed(
        t.MinFavorReceived.map(_.toFloat),
        t.MaxFavorReceived.map(_.toFloat),
      )
    }
  }

  given ProcessData[Objective.Types.UseItem] with {
    extension (t: Objective.Types.UseItem) {
      def process = Objective.Types.UseItemProcessed(
        t.ItemName map Content.byIname[Item],
        t.Target,
      )
    }
  }

  given ProcessData[Objective.Types.UseRecipe] with {
    extension (t: Objective.Types.UseRecipe) {
      def process = Objective.Types.UseRecipeProcessed(
        t.Skill map Info.byKey[Skill],
        t.Target map Content.byIname[Recipe],
      )
    }
  }

  given ProcessData[Objective.Types.UseAbility] with {
    extension (t: Objective.Types.UseAbility) {
      def process = Objective.Types.UseAbilityProcessed(
        Content.byIname[Ability](t.Target)
      )
    }
  }

  given ProcessData[Objective.Types.UseAbilityOnTargets] with {
    extension (t: Objective.Types.UseAbilityOnTargets) {
      def process = Objective.Types.UseAbilityOnTargetsProcessed(
        Info.byKey[Ai](t.Target),
        t.AbilityKeyword,
      )
    }
  }

  given ProcessData[Objective.Types.Have] with {
    extension (t: Objective.Types.Have) {
      def process = Objective.Types.HaveProcessed(
        t.ItemName map Content.byIname[Item],
        t.Target,
      )
    }
  }

  given ProcessData[Objective.Types.Special] with {
    extension (t: Objective.Types.Special) {
      def process = Objective.Types.SpecialProcessed(t.Target)
    }
  }

  given ProcessData[Objective.Types.UniqueSpecial] with {
    extension (t: Objective.Types.UniqueSpecial) {
      def process = Objective.Types.UniqueSpecialProcessed(
        t.Target
      )
    }
  }

  given ProcessData[Objective.Types.InteractionFlag] with {
    extension (t: Objective.Types.InteractionFlag) {
      def process = Objective.Types.InteractionFlagProcessed(
        t.Target
      )
    }
  }
  given ProcessData[Objective.Types.MultipleInteractionFlags] with {
    extension (t: Objective.Types.MultipleInteractionFlags) {
      def process =
        Objective.Types.MultipleInteractionFlagsProcessed(
          t.InteractionFlags
        )
    }
  }
  given ProcessData[Objective.Types.MeetRequirements] with {
    extension (t: Objective.Types.MeetRequirements) {
      def process = Objective.Types.MeetRequirementsProcessed()
    }
  }
  given ProcessData[Objective.Types.CompleteQuest] with {
    extension (t: Objective.Types.CompleteQuest) {
      def process = Objective.Types.CompleteQuestProcessed(
        Content.byIname[Quest](t.Target)
      )
    }
  }
  given ProcessData[Objective.Types.SayInChat] with {
    extension (t: Objective.Types.SayInChat) {
      def process = Objective.Types.SayInChatProcessed(
        t.Target
      )
    }
  }
  given ProcessData[Objective.Types.TipPlayer] with {
    extension (t: Objective.Types.TipPlayer) {
      def process = Objective.Types.TipPlayerProcessed(
        t.MinAmount.toInt
      )
    }
  }

// Requirements Types
}
