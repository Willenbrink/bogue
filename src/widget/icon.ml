open Label

class t ?font_size ?fg name =
  object
    (* see https://lab.artlung.com/font-awesome-sample/*)
    inherit Label.t ?font_size ?fg ~font:(File Theme.fa_font) (Theme.fa_symbol name)
  end
