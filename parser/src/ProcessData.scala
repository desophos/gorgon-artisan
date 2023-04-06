package gorgonartisan

import scala.annotation.experimental
import scala.compiletime.erasedValue
import scala.compiletime.summonInline
import scala.deriving.Mirror

import gorgonartisan.Quest.*

import cats.implicits.*

trait Processed[+A] // extends Product

trait ProcessData[IN] {
  type OUT <: Processed[IN]
  extension (in: IN) def process: OUT
}

// @experimental
object ProcessData {
  type Aux[I, O] = ProcessData[I] { type OUT = O }

  inline def pdProduct[FROM <: Product, TO <: Processed[FROM]](
      inline mTo: Mirror.ProductOf[TO]
  ): Aux[FROM, TO] =
    new ProcessData[FROM] {
      type OUT = TO
      extension (from: FROM) {
        def process = mTo.fromProduct(from)
      }
    }

  inline given derived[FROM <: Product, TO <: Processed[FROM]](using
      inline mFrom: Mirror.Of[FROM],
      inline mTo: Mirror.Of[TO],
  ): Aux[FROM, TO] =
    inline mFrom match {
      case mFrom: Mirror.ProductOf[FROM] =>
        mTo match {
          case mTo: Mirror.ProductOf[TO] =>
            pdProduct[FROM, TO](mTo)
        }
    }

// Info
  given Aux[Source, SourceProcessed] = new ProcessData[Source] {
    type OUT = SourceProcessed
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
  given Aux[Content, ContentProcessed] = new ProcessData[Content] {
    type OUT = ContentProcessed
    extension (content: Content) {
      def process = content match {
        case item: Item => summon[Aux[Item, ItemProcessed]].process(item)
        case recipe: Recipe =>
          summon[Aux[Recipe, RecipeProcessed]].process(recipe)
        case quest: Quest => summon[Aux[Quest, QuestProcessed]].process(quest)
      }
    }
  }

  given Aux[ItemStack, ItemStackProcessed] = new ProcessData[ItemStack] {
    type OUT = ItemStackProcessed
    extension (itemStack: ItemStack) {
      def process: ItemStackProcessed = ItemStackProcessed(
        itemStack.ChanceToConsume,
        itemStack.Desc,
        itemStack.ItemCode.map(Content.byId[Item]),
        itemStack.ItemKeys,
        itemStack.StackSize,
      )
    }
  }

  given Aux[Recipe, RecipeProcessed] = new ProcessData[Recipe] {
    type OUT = RecipeProcessed
    extension (recipe: Recipe) {
      def process: RecipeProcessed = RecipeProcessed(
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

  given Aux[Item, ItemProcessed] = new ProcessData[Item] {
    type OUT = ItemProcessed
    extension (item: Item) {
      def process: ItemProcessed = ItemProcessed(
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

  given Aux[Quest, QuestProcessed] = new ProcessData[Quest] {
    type OUT = QuestProcessed
    extension (quest: Quest) {
      def process: QuestProcessed = QuestProcessed(
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
  given Aux[Reward, RewardProcessed] = new ProcessData[Reward] {
    type OUT = RewardProcessed
    extension (reward: Reward) {
      def process = reward match {
        case r: Reward.SkillXp =>
          summon[Aux[Reward.SkillXp, Reward.SkillXpProcessed]].process(r)
        case r: Reward.CombatXp =>
          summon[Aux[Reward.CombatXp, Reward.CombatXpProcessed]].process(r)
        case r: Reward.GuildXp =>
          summon[Aux[Reward.GuildXp, Reward.GuildXpProcessed]].process(r)
        case r: Reward.GuildCredits =>
          summon[Aux[Reward.GuildCredits, Reward.GuildCreditsProcessed]]
            .process(r)
        case r: Reward.Currency =>
          summon[Aux[Reward.Currency, Reward.CurrencyProcessed]].process(r)
        case r: Reward.WorkOrderCurrency =>
          summon[Aux[Reward.WorkOrderCurrency, Reward.WorkOrderCurrencyProcessed]]
            .process(r)
        case r: Reward.Recipe =>
          summon[Aux[Reward.Recipe, Reward.RecipeProcessed]].process(r)
        case r: Reward.Ability =>
          summon[Aux[Reward.Ability, Reward.AbilityProcessed]].process(r)
      }
    }
  }
  given Aux[Reward.SkillXp, Reward.SkillXpProcessed] =
    new ProcessData[Reward.SkillXp] {
      type OUT = Reward.SkillXpProcessed
      extension (r: Reward.SkillXp) {
        def process: Reward.SkillXpProcessed =
          Reward.SkillXpProcessed(Info.byKey[Skill](r.Skill), r.Xp)
      }
    }
  given Aux[Reward.CombatXp, Reward.CombatXpProcessed] =
    new ProcessData[Reward.CombatXp] {
      type OUT = Reward.CombatXpProcessed
      extension (r: Reward.CombatXp) {
        def process: Reward.CombatXpProcessed = Reward.CombatXpProcessed(r.Xp)
      }
    }
  given Aux[Reward.GuildXp, Reward.GuildXpProcessed] =
    new ProcessData[Reward.GuildXp] {
      type OUT = Reward.GuildXpProcessed
      extension (r: Reward.GuildXp) {
        def process: Reward.GuildXpProcessed = Reward.GuildXpProcessed(r.Xp)
      }
    }
  given Aux[Reward.GuildCredits, Reward.GuildCreditsProcessed] =
    new ProcessData[Reward.GuildCredits] {
      type OUT = Reward.GuildCreditsProcessed
      extension (r: Reward.GuildCredits) {
        def process: Reward.GuildCreditsProcessed =
          Reward.GuildCreditsProcessed(r.Credits)
      }
    }
  given Aux[Reward.Currency, Reward.CurrencyProcessed] =
    new ProcessData[Reward.Currency] {
      type OUT = Reward.CurrencyProcessed
      extension (r: Reward.Currency) {
        def process: Reward.CurrencyProcessed =
          Reward.CurrencyProcessed(r.Amount, r.Currency)
      }
    }
  given Aux[Reward.WorkOrderCurrency, Reward.WorkOrderCurrencyProcessed] =
    new ProcessData[Reward.WorkOrderCurrency] {
      type OUT = Reward.WorkOrderCurrencyProcessed
      extension (r: Reward.WorkOrderCurrency) {
        def process: Reward.WorkOrderCurrencyProcessed =
          Reward.WorkOrderCurrencyProcessed(
            r.Amount,
            r.Currency,
          )
      }
    }
  given pRewardRecipe: Aux[Reward.Recipe, Reward.RecipeProcessed] =
    new ProcessData[Reward.Recipe] {
      type OUT = Reward.RecipeProcessed
      extension (r: Reward.Recipe) {
        def process: Reward.RecipeProcessed =
          Reward.RecipeProcessed(Content.byIname[Recipe](r.Recipe))
      }
    }

  given Aux[Reward.Ability, Reward.AbilityProcessed] =
    new ProcessData[Reward.Ability] {
      type OUT = Reward.AbilityProcessed
      extension (r: Reward.Ability) {
        def process: Reward.AbilityProcessed =
          Reward.AbilityProcessed(Content.byIname[Ability](r.Ability))
      }
    }

  given Aux[Requirement, RequirementProcessed] = new ProcessData[Requirement] {
    type OUT = RequirementProcessed
    extension (req: Requirement) {
      def process = req match {
        case r: Requirement.MinFavor =>
          summon[Aux[Requirement.MinFavor, Requirement.MinFavorProcessed]]
            .process(r)
        case r: Requirement.MinFavorLevel =>
          summon[
            Aux[Requirement.MinFavorLevel, Requirement.MinFavorLevelProcessed]
          ].process(r)
        case r: Requirement.MinSkillLevel =>
          summon[
            Aux[Requirement.MinSkillLevel, Requirement.MinSkillLevelProcessed]
          ].process(r)
        case r: Requirement.ActiveCombatSkill =>
          summon[Aux[
            Requirement.ActiveCombatSkill,
            Requirement.ActiveCombatSkillProcessed,
          ]].process(r)
        case r: Requirement.EquipmentSlotEmpty =>
          summon[Aux[
            Requirement.EquipmentSlotEmpty,
            Requirement.EquipmentSlotEmptyProcessed,
          ]].process(r)
        case r: Requirement.InteractionFlagSet =>
          summon[Aux[
            Requirement.InteractionFlagSet,
            Requirement.InteractionFlagSetProcessed,
          ]].process(r)
        case r: Requirement.InteractionFlagUnset =>
          summon[Aux[
            Requirement.InteractionFlagUnset,
            Requirement.InteractionFlagUnsetProcessed,
          ]].process(r)
        case r: Requirement.QuestCompleted =>
          summon[
            Aux[Requirement.QuestCompleted, Requirement.QuestCompletedProcessed]
          ].process(r)
        case r: Requirement.QuestCompletedRecently =>
          summon[Aux[
            Requirement.QuestCompletedRecently,
            Requirement.QuestCompletedRecentlyProcessed,
          ]].process(r)
        case r: Requirement.GuildQuestCompleted =>
          summon[Aux[
            Requirement.GuildQuestCompleted,
            Requirement.GuildQuestCompletedProcessed,
          ]].process(r)
        case r: Requirement.AreaEventOff =>
          summon[Aux[Requirement.AreaEventOff, Requirement.AreaEventOffProcessed]]
            .process(r)
        case r: Requirement.AreaEventOn =>
          summon[Aux[Requirement.AreaEventOn, Requirement.AreaEventOnProcessed]]
            .process(r)
        case r: Requirement.IsWarden =>
          summon[Aux[Requirement.IsWarden, Requirement.IsWardenProcessed]]
            .process(r)
        case r: Requirement.IsLongtimeAnimal =>
          summon[Aux[
            Requirement.IsLongtimeAnimal,
            Requirement.IsLongtimeAnimalProcessed,
          ]].process(r)
        case r: Requirement.GeneralShape =>
          summon[Aux[Requirement.GeneralShape, Requirement.GeneralShapeProcessed]]
            .process(r)
        case r: Requirement.MoonPhase =>
          summon[Aux[Requirement.MoonPhase, Requirement.MoonPhaseProcessed]]
            .process(r)
        case r: Requirement.RuntimeBehaviorRuleSet =>
          summon[Aux[
            Requirement.RuntimeBehaviorRuleSet,
            Requirement.RuntimeBehaviorRuleSetProcessed,
          ]].process(r)
        case r: Requirement.HangOutCompleted =>
          summon[Aux[
            Requirement.HangOutCompleted,
            Requirement.HangOutCompletedProcessed,
          ]].process(r)
        case r: Requirement.Race =>
          summon[Aux[Requirement.Race, Requirement.RaceProcessed]].process(r)
        case r: Requirement.Or =>
          summon[Aux[Requirement.Or, Requirement.OrProcessed]].process(r)
        case r: Requirement.HasEffectKeyword =>
          summon[Aux[
            Requirement.HasEffectKeyword,
            Requirement.HasEffectKeywordProcessed,
          ]].process(r)
        case r: Requirement.Appearance =>
          summon[Aux[Requirement.Appearance, Requirement.AppearanceProcessed]]
            .process(r)
        case r: Requirement.TimeOfDay =>
          summon[Aux[Requirement.TimeOfDay, Requirement.TimeOfDayProcessed]]
            .process(r)
        case r: Requirement.PetCount =>
          summon[Aux[Requirement.PetCount, Requirement.PetCountProcessed]]
            .process(r)
        case r: Requirement.EntityPhysicalState =>
          summon[Aux[
            Requirement.EntityPhysicalState,
            Requirement.EntityPhysicalStateProcessed,
          ]].process(r)
        case r: Requirement.ScriptAtomicMatches =>
          summon[Aux[
            Requirement.ScriptAtomicMatches,
            Requirement.ScriptAtomicMatchesProcessed,
          ]].process(r)
      }
    }
  }

  given Aux[Requirement.MinFavor, Requirement.MinFavorProcessed] =
    new ProcessData[Requirement.MinFavor] {
      type OUT = Requirement.MinFavorProcessed
      extension (r: Requirement.MinFavor) {
        def process: Requirement.MinFavorProcessed =
          Requirement.MinFavorProcessed(r.MinFavor, Info.byNpcName(r.Npc))
      }
    }

  given Aux[Requirement.MinFavorLevel, Requirement.MinFavorLevelProcessed] =
    new ProcessData[Requirement.MinFavorLevel] {
      type OUT = Requirement.MinFavorLevelProcessed
      extension (r: Requirement.MinFavorLevel) {
        def process =
          Requirement.MinFavorLevelProcessed(r.Level, Info.byNpcName(r.Npc))
      }
    }

  given Aux[Requirement.MinSkillLevel, Requirement.MinSkillLevelProcessed] =
    new ProcessData[Requirement.MinSkillLevel] {
      type OUT = Requirement.MinSkillLevelProcessed
      extension (r: Requirement.MinSkillLevel) {
        def process =
          Requirement.MinSkillLevelProcessed(
            r.Level,
            Info.byKey[Skill](r.Skill),
          )
      }
    }

  given Aux[
    Requirement.ActiveCombatSkill,
    Requirement.ActiveCombatSkillProcessed,
  ] = new ProcessData[Requirement.ActiveCombatSkill] {
    type OUT = Requirement.ActiveCombatSkillProcessed
    extension (r: Requirement.ActiveCombatSkill) {
      def process =
        Requirement.ActiveCombatSkillProcessed(Info.byKey[Skill](r.Skill))
    }
  }

  given Aux[
    Requirement.EquipmentSlotEmpty,
    Requirement.EquipmentSlotEmptyProcessed,
  ] = ProcessData.derived[
    Requirement.EquipmentSlotEmpty,
    Requirement.EquipmentSlotEmptyProcessed,
  ]

  given Aux[
    Requirement.InteractionFlagSet,
    Requirement.InteractionFlagSetProcessed,
  ] = ProcessData.derived[
    Requirement.InteractionFlagSet,
    Requirement.InteractionFlagSetProcessed,
  ]

  given Aux[
    Requirement.InteractionFlagUnset,
    Requirement.InteractionFlagUnsetProcessed,
  ] = ProcessData.derived[
    Requirement.InteractionFlagUnset,
    Requirement.InteractionFlagUnsetProcessed,
  ]

  given Aux[Requirement.QuestCompleted, Requirement.QuestCompletedProcessed] =
    new ProcessData[Requirement.QuestCompleted] {
      type OUT = Requirement.QuestCompletedProcessed
      extension (r: Requirement.QuestCompleted) {
        def process: Requirement.QuestCompletedProcessed =
          Requirement.QuestCompletedProcessed(Content.byIname(r.Quest))
      }
    }

  given Aux[
    Requirement.QuestCompletedRecently,
    Requirement.QuestCompletedRecentlyProcessed,
  ] = new ProcessData[Requirement.QuestCompletedRecently] {
    type OUT = Requirement.QuestCompletedRecentlyProcessed
    extension (r: Requirement.QuestCompletedRecently) {
      def process =
        Requirement.QuestCompletedRecentlyProcessed(Content.byIname(r.Quest))
    }
  }

  given Aux[
    Requirement.GuildQuestCompleted,
    Requirement.GuildQuestCompletedProcessed,
  ] = new ProcessData[Requirement.GuildQuestCompleted] {
    type OUT = Requirement.GuildQuestCompletedProcessed
    extension (r: Requirement.GuildQuestCompleted) {
      def process =
        Requirement.GuildQuestCompletedProcessed(Content.byIname(r.Quest))
    }
  }

  given Aux[Requirement.AreaEventOff, Requirement.AreaEventOffProcessed] =
    ProcessData.derived[
      Requirement.AreaEventOff,
      Requirement.AreaEventOffProcessed,
    ]

  given Aux[Requirement.AreaEventOn, Requirement.AreaEventOnProcessed] =
    ProcessData.derived[
      Requirement.AreaEventOn,
      Requirement.AreaEventOnProcessed,
    ]

  given Aux[Requirement.IsWarden, Requirement.IsWardenProcessed] =
    ProcessData.derived[
      Requirement.IsWarden,
      Requirement.IsWardenProcessed,
    ]

  given Aux[Requirement.IsLongtimeAnimal, Requirement.IsLongtimeAnimalProcessed] =
    ProcessData.derived[
      Requirement.IsLongtimeAnimal,
      Requirement.IsLongtimeAnimalProcessed,
    ]

  given Aux[Requirement.GeneralShape, Requirement.GeneralShapeProcessed] =
    ProcessData.derived[
      Requirement.GeneralShape,
      Requirement.GeneralShapeProcessed,
    ]

  given Aux[Requirement.MoonPhase, Requirement.MoonPhaseProcessed] =
    ProcessData.derived[
      Requirement.MoonPhase,
      Requirement.MoonPhaseProcessed,
    ]

  given Aux[
    Requirement.RuntimeBehaviorRuleSet,
    Requirement.RuntimeBehaviorRuleSetProcessed,
  ] = ProcessData.derived[
    Requirement.RuntimeBehaviorRuleSet,
    Requirement.RuntimeBehaviorRuleSetProcessed,
  ]

  given Aux[Requirement.HangOutCompleted, Requirement.HangOutCompletedProcessed] =
    ProcessData.derived[
      Requirement.HangOutCompleted,
      Requirement.HangOutCompletedProcessed,
    ]

  given Aux[Requirement.Race, Requirement.RaceProcessed] =
    ProcessData.derived[
      Requirement.Race,
      Requirement.RaceProcessed,
    ]

  given Aux[Requirement.Or, Requirement.OrProcessed] =
    new ProcessData[Requirement.Or] {
      type OUT = Requirement.OrProcessed
      extension (r: Requirement.Or) {
        def process: Requirement.OrProcessed =
          Requirement.OrProcessed(r.List.map(_.process))
      }
    }

  given Aux[Requirement.HasEffectKeyword, Requirement.HasEffectKeywordProcessed] =
    ProcessData.derived[
      Requirement.HasEffectKeyword,
      Requirement.HasEffectKeywordProcessed,
    ]

  given Aux[Requirement.Appearance, Requirement.AppearanceProcessed] =
    ProcessData.derived[
      Requirement.Appearance,
      Requirement.AppearanceProcessed,
    ]

  given Aux[Requirement.TimeOfDay, Requirement.TimeOfDayProcessed] =
    ProcessData.derived[
      Requirement.TimeOfDay,
      Requirement.TimeOfDayProcessed,
    ]

  given Aux[Requirement.PetCount, Requirement.PetCountProcessed] =
    ProcessData.derived[
      Requirement.PetCount,
      Requirement.PetCountProcessed,
    ]

  given Aux[
    Requirement.EntityPhysicalState,
    Requirement.EntityPhysicalStateProcessed,
  ] = ProcessData.derived[
    Requirement.EntityPhysicalState,
    Requirement.EntityPhysicalStateProcessed,
  ]

  given Aux[
    Requirement.ScriptAtomicMatches,
    Requirement.ScriptAtomicMatchesProcessed,
  ] = ProcessData.derived[
    Requirement.ScriptAtomicMatches,
    Requirement.ScriptAtomicMatchesProcessed,
  ]

  given Aux[Objective, ObjectiveProcessed] = new ProcessData[Objective] {
    type OUT = ObjectiveProcessed
    extension (t: Objective) {
      def process: ObjectiveProcessed = ObjectiveProcessed(
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
  given Aux[Objective.Types, Objective.TypesProcessed] =
    new ProcessData[Objective.Types] {
      type OUT = Objective.TypesProcessed
      extension (ot: Objective.Types) {
        def process = ot match {
          case t: Objective.Types.Collect =>
            summon[Aux[Objective.Types.Collect, Objective.Types.CollectProcessed]]
              .process(t)
          case t: Objective.Types.Scripted =>
            summon[
              Aux[Objective.Types.Scripted, Objective.Types.ScriptedProcessed]
            ].process(t)
          case t: Objective.Types.ScriptedReceiveItem =>
            summon[Aux[
              Objective.Types.ScriptedReceiveItem,
              Objective.Types.ScriptedReceiveItemProcessed,
            ]].process(t)
          case t: Objective.Types.ScriptAtomicInt =>
            summon[Aux[
              Objective.Types.ScriptAtomicInt,
              Objective.Types.ScriptAtomicIntProcessed,
            ]].process(t)
          case t: Objective.Types.Kill =>
            summon[Aux[Objective.Types.Kill, Objective.Types.KillProcessed]]
              .process(t)
          case t: Objective.Types.KillElite =>
            summon[
              Aux[Objective.Types.KillElite, Objective.Types.KillEliteProcessed]
            ].process(t)
          case t: Objective.Types.Harvest =>
            summon[Aux[Objective.Types.Harvest, Objective.Types.HarvestProcessed]]
              .process(t)
          case t: Objective.Types.Loot =>
            summon[Aux[Objective.Types.Loot, Objective.Types.LootProcessed]]
              .process(t)
          case t: Objective.Types.BeAttacked =>
            summon[Aux[
              Objective.Types.BeAttacked,
              Objective.Types.BeAttackedProcessed,
            ]].process(t)
          case t: Objective.Types.Bury =>
            summon[Aux[Objective.Types.Bury, Objective.Types.BuryProcessed]]
              .process(t)
          case t: Objective.Types.Deliver =>
            summon[Aux[Objective.Types.Deliver, Objective.Types.DeliverProcessed]]
              .process(t)
          case t: Objective.Types.DruidKill =>
            summon[
              Aux[Objective.Types.DruidKill, Objective.Types.DruidKillProcessed]
            ].process(t)
          case t: Objective.Types.DruidScripted =>
            summon[Aux[
              Objective.Types.DruidScripted,
              Objective.Types.DruidScriptedProcessed,
            ]].process(t)
          case t: Objective.Types.GuildKill =>
            summon[
              Aux[Objective.Types.GuildKill, Objective.Types.GuildKillProcessed]
            ].process(t)
          case t: Objective.Types.GuildGiveItem =>
            summon[Aux[
              Objective.Types.GuildGiveItem,
              Objective.Types.GuildGiveItemProcessed,
            ]].process(t)
          case t: Objective.Types.GiveGift =>
            summon[
              Aux[Objective.Types.GiveGift, Objective.Types.GiveGiftProcessed]
            ].process(t)
          case t: Objective.Types.UseItem =>
            summon[Aux[Objective.Types.UseItem, Objective.Types.UseItemProcessed]]
              .process(t)
          case t: Objective.Types.UseRecipe =>
            summon[
              Aux[Objective.Types.UseRecipe, Objective.Types.UseRecipeProcessed]
            ].process(t)
          case t: Objective.Types.UseAbility =>
            summon[Aux[
              Objective.Types.UseAbility,
              Objective.Types.UseAbilityProcessed,
            ]].process(t)
          case t: Objective.Types.UseAbilityOnTargets =>
            summon[Aux[
              Objective.Types.UseAbilityOnTargets,
              Objective.Types.UseAbilityOnTargetsProcessed,
            ]].process(t)
          case t: Objective.Types.Have =>
            summon[Aux[Objective.Types.Have, Objective.Types.HaveProcessed]]
              .process(t)
          case t: Objective.Types.Special =>
            summon[Aux[Objective.Types.Special, Objective.Types.SpecialProcessed]]
              .process(t)
          case t: Objective.Types.UniqueSpecial =>
            summon[Aux[
              Objective.Types.UniqueSpecial,
              Objective.Types.UniqueSpecialProcessed,
            ]].process(t)
          case t: Objective.Types.InteractionFlag =>
            summon[Aux[
              Objective.Types.InteractionFlag,
              Objective.Types.InteractionFlagProcessed,
            ]].process(t)
          case t: Objective.Types.MultipleInteractionFlags =>
            summon[Aux[
              Objective.Types.MultipleInteractionFlags,
              Objective.Types.MultipleInteractionFlagsProcessed,
            ]]
              .process(t)
          case t: Objective.Types.MeetRequirements =>
            summon[Aux[
              Objective.Types.MeetRequirements,
              Objective.Types.MeetRequirementsProcessed,
            ]].process(t)
          case t: Objective.Types.CompleteQuest =>
            summon[Aux[
              Objective.Types.CompleteQuest,
              Objective.Types.CompleteQuestProcessed,
            ]].process(t)
          case t: Objective.Types.SayInChat =>
            summon[
              Aux[Objective.Types.SayInChat, Objective.Types.SayInChatProcessed]
            ].process(t)
          case t: Objective.Types.TipPlayer =>
            summon[
              Aux[Objective.Types.TipPlayer, Objective.Types.TipPlayerProcessed]
            ].process(t)
        }
      }
    }

  given Aux[Objective.Types.Collect, Objective.Types.CollectProcessed] =
    new ProcessData[Objective.Types.Collect] {
      type OUT = Objective.Types.CollectProcessed
      extension (t: Objective.Types.Collect) {
        def process: Objective.Types.CollectProcessed =
          Objective.Types.CollectProcessed(
            t.ItemName.map(Content.byIname[Item]),
            t.Target,
          )
      }
    }

  given Aux[Objective.Types.Scripted, Objective.Types.ScriptedProcessed] =
    new ProcessData[Objective.Types.Scripted] {
      type OUT = Objective.Types.ScriptedProcessed
      extension (t: Objective.Types.Scripted) {
        def process: Objective.Types.ScriptedProcessed =
          Objective.Types.ScriptedProcessed()
      }
    }

  given Aux[
    Objective.Types.ScriptedReceiveItem,
    Objective.Types.ScriptedReceiveItemProcessed,
  ] = new ProcessData[Objective.Types.ScriptedReceiveItem] {
    type OUT = Objective.Types.ScriptedReceiveItemProcessed
    extension (t: Objective.Types.ScriptedReceiveItem) {
      def process: Objective.Types.ScriptedReceiveItemProcessed =
        Objective.Types.ScriptedReceiveItemProcessed(
          Content.byIname(t.Item),
          Info.byNpcName(t.Target),
        )
    }
  }

  given Aux[
    Objective.Types.ScriptAtomicInt,
    Objective.Types.ScriptAtomicIntProcessed,
  ] = new ProcessData[Objective.Types.ScriptAtomicInt] {
    type OUT = Objective.Types.ScriptAtomicIntProcessed
    extension (t: Objective.Types.ScriptAtomicInt) {
      def process: Objective.Types.ScriptAtomicIntProcessed =
        Objective.Types.ScriptAtomicIntProcessed(
          t.Target: String
        )
    }
  }

  given Aux[Objective.Types.Kill, Objective.Types.KillProcessed] =
    new ProcessData[Objective.Types.Kill] {
      type OUT = Objective.Types.KillProcessed
      extension (t: Objective.Types.Kill) {
        def process: Objective.Types.KillProcessed =
          Objective.Types.KillProcessed(
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

  given Aux[Objective.Types.KillElite, Objective.Types.KillEliteProcessed] =
    new ProcessData[Objective.Types.KillElite] {
      type OUT = Objective.Types.KillEliteProcessed
      extension (t: Objective.Types.KillElite) {
        def process: Objective.Types.KillEliteProcessed =
          Objective.Types.KillEliteProcessed(
            t.Target
          )
      }
    }

  given Aux[Objective.Types.Harvest, Objective.Types.HarvestProcessed] =
    new ProcessData[Objective.Types.Harvest] {
      type OUT = Objective.Types.HarvestProcessed
      extension (t: Objective.Types.Harvest) {
        def process: Objective.Types.HarvestProcessed =
          Objective.Types.HarvestProcessed(
            t.ItemName map Content.byIname[Item],
            t.Target,
          )
      }
    }

  given Aux[Objective.Types.Loot, Objective.Types.LootProcessed] =
    new ProcessData[Objective.Types.Loot] {
      type OUT = Objective.Types.LootProcessed
      extension (t: Objective.Types.Loot) {
        def process: Objective.Types.LootProcessed =
          Objective.Types.LootProcessed(
            t.ItemName map Content.byIname[Item],
            t.Target,
            t.MonsterTypeTag,
          )
      }
    }

  given Aux[Objective.Types.BeAttacked, Objective.Types.BeAttackedProcessed] =
    new ProcessData[Objective.Types.BeAttacked] {
      type OUT = Objective.Types.BeAttackedProcessed
      extension (t: Objective.Types.BeAttacked) {
        def process: Objective.Types.BeAttackedProcessed =
          Objective.Types.BeAttackedProcessed(
            t.AnatomyType,
            t.Target map Info.byKey[Ai],
          )
      }
    }

  given Aux[Objective.Types.Bury, Objective.Types.BuryProcessed] =
    new ProcessData[Objective.Types.Bury] {
      type OUT = Objective.Types.BuryProcessed
      extension (t: Objective.Types.Bury) {
        def process: Objective.Types.BuryProcessed =
          Objective.Types.BuryProcessed(
            t.AnatomyType,
            t.Target map Info.byKey[Ai],
          )
      }
    }

  given Aux[Objective.Types.Deliver, Objective.Types.DeliverProcessed] =
    new ProcessData[Objective.Types.Deliver] {
      type OUT = Objective.Types.DeliverProcessed
      extension (t: Objective.Types.Deliver) {
        def process: Objective.Types.DeliverProcessed =
          Objective.Types.DeliverProcessed(
            Content.byIname[Item](t.ItemName),
            t.NumToDeliver.map(_.toInt).getOrElse(1),
            Info.byNpcName(t.Target),
          )
      }
    }

  given Aux[Objective.Types.DruidKill, Objective.Types.DruidKillProcessed] =
    new ProcessData[Objective.Types.DruidKill] {
      type OUT = Objective.Types.DruidKillProcessed
      extension (t: Objective.Types.DruidKill) {
        def process: Objective.Types.DruidKillProcessed =
          Objective.Types.DruidKillProcessed(
            t.Target
          )
      }
    }

  given Aux[
    Objective.Types.DruidScripted,
    Objective.Types.DruidScriptedProcessed,
  ] = new ProcessData[Objective.Types.DruidScripted] {
    type OUT = Objective.Types.DruidScriptedProcessed
    extension (t: Objective.Types.DruidScripted) {
      def process: Objective.Types.DruidScriptedProcessed =
        Objective.Types.DruidScriptedProcessed(
          t.Target
        )
    }
  }

  given Aux[Objective.Types.GuildKill, Objective.Types.GuildKillProcessed] =
    new ProcessData[Objective.Types.GuildKill] {
      type OUT = Objective.Types.GuildKillProcessed
      extension (t: Objective.Types.GuildKill) {
        def process: Objective.Types.GuildKillProcessed =
          Objective.Types.GuildKillProcessed(
            t.Target
          )
      }
    }

  given Aux[
    Objective.Types.GuildGiveItem,
    Objective.Types.GuildGiveItemProcessed,
  ] = new ProcessData[Objective.Types.GuildGiveItem] {
    type OUT = Objective.Types.GuildGiveItemProcessed
    extension (t: Objective.Types.GuildGiveItem) {
      def process: Objective.Types.GuildGiveItemProcessed =
        Objective.Types.GuildGiveItemProcessed(
          t.ItemName map Content.byIname[Item],
          t.ItemKeyword: Option[String], // Keyword
          Info.byNpcName(t.Target),
        )
    }
  }

  given Aux[Objective.Types.GiveGift, Objective.Types.GiveGiftProcessed] =
    new ProcessData[Objective.Types.GiveGift] {
      type OUT = Objective.Types.GiveGiftProcessed
      extension (t: Objective.Types.GiveGift) {
        def process: Objective.Types.GiveGiftProcessed =
          Objective.Types.GiveGiftProcessed(
            t.MinFavorReceived.map(_.toFloat),
            t.MaxFavorReceived.map(_.toFloat),
          )
      }
    }

  given Aux[Objective.Types.UseItem, Objective.Types.UseItemProcessed] =
    new ProcessData[Objective.Types.UseItem] {
      type OUT = Objective.Types.UseItemProcessed
      extension (t: Objective.Types.UseItem) {
        def process: Objective.Types.UseItemProcessed =
          Objective.Types.UseItemProcessed(
            t.ItemName map Content.byIname[Item],
            t.Target,
          )
      }
    }

  given Aux[Objective.Types.UseRecipe, Objective.Types.UseRecipeProcessed] =
    new ProcessData[Objective.Types.UseRecipe] {
      type OUT = Objective.Types.UseRecipeProcessed
      extension (t: Objective.Types.UseRecipe) {
        def process: Objective.Types.UseRecipeProcessed =
          Objective.Types.UseRecipeProcessed(
            t.Skill map Info.byKey[Skill],
            t.Target map Content.byIname[Recipe],
          )
      }
    }

  given Aux[Objective.Types.UseAbility, Objective.Types.UseAbilityProcessed] =
    new ProcessData[Objective.Types.UseAbility] {
      type OUT = Objective.Types.UseAbilityProcessed
      extension (t: Objective.Types.UseAbility) {
        def process: Objective.Types.UseAbilityProcessed =
          Objective.Types.UseAbilityProcessed(
            Content.byIname[Ability](t.Target)
          )
      }
    }

  given Aux[
    Objective.Types.UseAbilityOnTargets,
    Objective.Types.UseAbilityOnTargetsProcessed,
  ] = new ProcessData[Objective.Types.UseAbilityOnTargets] {
    type OUT = Objective.Types.UseAbilityOnTargetsProcessed
    extension (t: Objective.Types.UseAbilityOnTargets) {
      def process: Objective.Types.UseAbilityOnTargetsProcessed =
        Objective.Types.UseAbilityOnTargetsProcessed(
          Info.byKey[Ai](t.Target),
          t.AbilityKeyword,
        )
    }
  }

  given Aux[Objective.Types.Have, Objective.Types.HaveProcessed] =
    new ProcessData[Objective.Types.Have] {
      type OUT = Objective.Types.HaveProcessed
      extension (t: Objective.Types.Have) {
        def process: Objective.Types.HaveProcessed =
          Objective.Types.HaveProcessed(
            t.ItemName map Content.byIname[Item],
            t.Target,
          )
      }
    }

  given Aux[Objective.Types.Special, Objective.Types.SpecialProcessed] =
    new ProcessData[Objective.Types.Special] {
      type OUT = Objective.Types.SpecialProcessed
      extension (t: Objective.Types.Special) {
        def process: Objective.Types.SpecialProcessed =
          Objective.Types.SpecialProcessed(t.Target)
      }
    }

  given Aux[
    Objective.Types.UniqueSpecial,
    Objective.Types.UniqueSpecialProcessed,
  ] = new ProcessData[Objective.Types.UniqueSpecial] {
    type OUT = Objective.Types.UniqueSpecialProcessed
    extension (t: Objective.Types.UniqueSpecial) {
      def process: Objective.Types.UniqueSpecialProcessed =
        Objective.Types.UniqueSpecialProcessed(
          t.Target
        )
    }
  }

  given Aux[
    Objective.Types.InteractionFlag,
    Objective.Types.InteractionFlagProcessed,
  ] = new ProcessData[Objective.Types.InteractionFlag] {
    type OUT = Objective.Types.InteractionFlagProcessed
    extension (t: Objective.Types.InteractionFlag) {
      def process: Objective.Types.InteractionFlagProcessed =
        Objective.Types.InteractionFlagProcessed(
          t.Target
        )
    }
  }
  given Aux[
    Objective.Types.MultipleInteractionFlags,
    Objective.Types.MultipleInteractionFlagsProcessed,
  ] = new ProcessData[Objective.Types.MultipleInteractionFlags] {
    type OUT = Objective.Types.MultipleInteractionFlagsProcessed
    extension (t: Objective.Types.MultipleInteractionFlags) {
      def process =
        Objective.Types.MultipleInteractionFlagsProcessed(
          t.InteractionFlags
        )
    }
  }
  given Aux[
    Objective.Types.MeetRequirements,
    Objective.Types.MeetRequirementsProcessed,
  ] = new ProcessData[Objective.Types.MeetRequirements] {
    type OUT = Objective.Types.MeetRequirementsProcessed
    extension (t: Objective.Types.MeetRequirements) {
      def process: Objective.Types.MeetRequirementsProcessed =
        Objective.Types.MeetRequirementsProcessed()
    }
  }
  given Aux[
    Objective.Types.CompleteQuest,
    Objective.Types.CompleteQuestProcessed,
  ] = new ProcessData[Objective.Types.CompleteQuest] {
    type OUT = Objective.Types.CompleteQuestProcessed
    extension (t: Objective.Types.CompleteQuest) {
      def process: Objective.Types.CompleteQuestProcessed =
        Objective.Types.CompleteQuestProcessed(
          Content.byIname[Quest](t.Target)
        )
    }
  }
  given Aux[Objective.Types.SayInChat, Objective.Types.SayInChatProcessed] =
    new ProcessData[Objective.Types.SayInChat] {
      type OUT = Objective.Types.SayInChatProcessed
      extension (t: Objective.Types.SayInChat) {
        def process: Objective.Types.SayInChatProcessed =
          Objective.Types.SayInChatProcessed(
            t.Target
          )
      }
    }
  given Aux[Objective.Types.TipPlayer, Objective.Types.TipPlayerProcessed] =
    new ProcessData[Objective.Types.TipPlayer] {
      type OUT = Objective.Types.TipPlayerProcessed
      extension (t: Objective.Types.TipPlayer) {
        def process: Objective.Types.TipPlayerProcessed =
          Objective.Types.TipPlayerProcessed(
            t.MinAmount.toInt
          )
      }
    }

// Requirements Types
}
