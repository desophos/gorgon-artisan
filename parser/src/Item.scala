package gorgonartisan

case class Item(
    Behaviors: Option[List[Item.Behavior]],
    BestowQuest: Option[String],
    BestowRecipes: Option[List[String]],
    BestowTitle: Option[Int],
    CraftPoints: Option[Int],
    CraftingTargetLevel: Option[Int],
    Description: String,
    DroppedAppearance: Option[String],
    EffectDescs: Option[List[String]],
    EquipAppearance: Option[String],
    EquipSlot: Option[String],
    FoodDesc: Option[String],
    IconId: Int,
    InternalName: String,
    Keywords: Option[List[String]],
    Lint_VendorNpc: Option[String],
    MacGuffinQuestName: Option[String],
    MaxCarryable: Option[Int],
    MaxStackSize: Int,
    Name: String,
    NumUses: Option[Int],
    SkillReqs: Option[Map[String, Int]],
    StockDye: Option[String],
    TSysProfile: Option[String],
    Value: Float,
) extends Content

object Item {
  case class Behavior(
      UseAnimation: Option[String],
      UseDelay: Option[Float],
      UseDelayAnimation: Option[String],
      UseRequirements: Option[List[String]],
      UseVerb: String,
  )
}

case class ItemProcessed(
    Behaviors: Option[List[Item.Behavior]],
    BestowQuest: Option[Content.Ref[Quest]],
    BestowRecipes: Option[List[Content.Ref[Recipe]]],
    BestowTitle: Option[Int],
    CraftPoints: Option[Int],
    CraftingTargetLevel: Option[Int],
    Description: String,
    DroppedAppearance: Option[String],
    EffectDescs: Option[List[String]], // refs effects
    EquipAppearance: Option[String],
    EquipSlot: Option[String],
    FoodDesc: Option[String],
    IconId: Int,
    InternalName: String,
    Keywords: Option[List[String]], // refs Keyword
    Lint_VendorNpc: Option[Info.Ref[Npc]],
    MacGuffinQuestName: Option[Content.Ref[Quest]],
    MaxCarryable: Option[Int],
    MaxStackSize: Int,
    Name: String,
    NumUses: Option[Int],
    SkillReqs: Option[Map[Info.Ref[Skill], Int]],
    StockDye: Option[String],
    TSysProfile: Option[String], // refs tsysclientinfo
    Value: Float,
) extends Processed[Item]
