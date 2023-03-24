package gorgonartisan

case class Skill(
    AdvancementHints: Map[Int, String],
    AdvancementTable: Option[String], // refs advancementtables
    AuxCombat: Option[Boolean],
    Combat: Boolean,
    Description: String,
    GuestLevelCap: Int,
    Id: Int,
    InteractionFlagLevelCaps: Map[String, Int], // refs something, not sure
    IsFakeCombatSkill: Option[Boolean],
    MaxBonusLevels: Int,
    Name: String,
    Parents: Option[List[String]],                  // refs Skill
    RecipeIngredientKeywords: Option[List[String]], // refs Keyword
    Reports: Option[Map[Int, String]],
    Rewards: Map[Int, Skill.Reward],
    TSysCompatibleCombatSkills: List[String], // refs Skill
    XpTable: String,                          // refs xptables
) extends Info

object Skill {
  case class Reward(
      Ability: Option[String],
      BonusToSkill: Option[String],
      Notes: Option[String],
      Recipe: Option[String],
  )

  case class RewardProcessed(
      Ability: Option[String], // refs Ability
      BonusToSkill: Option[Info.Ref[Skill]],
      Notes: Option[String],
      Recipe: Option[Content.Ref[Recipe]],
  ) extends Processed[Reward]
}
