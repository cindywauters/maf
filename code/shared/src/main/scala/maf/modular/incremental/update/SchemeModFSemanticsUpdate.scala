package maf.modular.incremental.update

import maf.language.change.CodeVersion.{Old, New}
import maf.language.scheme.{SchemeBody, SchemeExp}
import maf.modular.scheme.modf.SchemeModFComponent.{Call, Main}
import maf.modular.scheme.modf.{SchemeModFComponent, SchemeModFSemanticsM}

trait SchemeModFSemanticsUpdate extends SchemeModFSemanticsM with IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp]:

  lazy val secondMainBody = secondProgram

  override def body(cmp: SchemeModFComponent): SchemeExp = cmp match
    case Main if version == Old  =>  mainBody
    case Main if version == New  =>  secondMainBody
    case c: Call[_]              => SchemeBody(c.lambda.body)
