let drag_icon_xpm = [|
    "36 48 9 1";
    " 	c None";
    ".	c #020204";
    "+	c #8F8F90";
    "@	c #D3D3D2";
    "#	c #AEAEAC";
    "$	c #ECECEC";
    "%	c #A2A2A4";
    "&	c #FEFEFC";
    "*	c #BEBEBC";
    "               .....................";
    "              ..&&&&&&&&&&&&&&&&&&&.";
    "             ...&&&&&&&&&&&&&&&&&&&.";
    "            ..&.&&&&&&&&&&&&&&&&&&&.";
    "           ..&&.&&&&&&&&&&&&&&&&&&&.";
    "          ..&&&.&&&&&&&&&&&&&&&&&&&.";
    "         ..&&&&.&&&&&&&&&&&&&&&&&&&.";
    "        ..&&&&&.&&&@&&&&&&&&&&&&&&&.";
    "       ..&&&&&&.*$%$+$&&&&&&&&&&&&&.";
    "      ..&&&&&&&.%$%$+&&&&&&&&&&&&&&.";
    "     ..&&&&&&&&.#&#@$&&&&&&&&&&&&&&.";
    "    ..&&&&&&&&&.#$**#$&&&&&&&&&&&&&.";
    "   ..&&&&&&&&&&.&@%&%$&&&&&&&&&&&&&.";
    "  ..&&&&&&&&&&&.&&&&&&&&&&&&&&&&&&&.";
    " ..&&&&&&&&&&&&.&&&&&&&&&&&&&&&&&&&.";
    "................&$@&&&@&&&&&&&&&&&&.";
    ".&&&&&&&+&&#@%#+@#@*$%$+$&&&&&&&&&&.";
    ".&&&&&&&+&&#@#@&&@*%$%$+&&&&&&&&&&&.";
    ".&&&&&&&+&$%&#@&#@@#&#@$&&&&&&&&&&&.";
    ".&&&&&&@#@@$&*@&@#@#$**#$&&&&&&&&&&.";
    ".&&&&&&&&&&&&&&&&&&&@%&%$&&&&&&&&&&.";
    ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
    ".&&&&&&&&$#@@$&&&&&&&&&&&&&&&&&&&&&.";
    ".&&&&&&&&&+&$+&$&@&$@&&$@&&&&&&&&&&.";
    ".&&&&&&&&&+&&#@%#+@#@*$%&+$&&&&&&&&.";
    ".&&&&&&&&&+&&#@#@&&@*%$%$+&&&&&&&&&.";
    ".&&&&&&&&&+&$%&#@&#@@#&#@$&&&&&&&&&.";
    ".&&&&&&&&@#@@$&*@&@#@#$#*#$&&&&&&&&.";
    ".&&&&&&&&&&&&&&&&&&&&&$%&%$&&&&&&&&.";
    ".&&&&&&&&&&$#@@$&&&&&&&&&&&&&&&&&&&.";
    ".&&&&&&&&&&&+&$%&$$@&$@&&$@&&&&&&&&.";
    ".&&&&&&&&&&&+&&#@%#+@#@*$%$+$&&&&&&.";
    ".&&&&&&&&&&&+&&#@#@&&@*#$%$+&&&&&&&.";
    ".&&&&&&&&&&&+&$+&*@&#@@#&#@$&&&&&&&.";
    ".&&&&&&&&&&$%@@&&*@&@#@#$#*#&&&&&&&.";
    ".&&&&&&&&&&&&&&&&&&&&&&&$%&%$&&&&&&.";
    ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
    ".&&&&&&&&&&&&&&$#@@$&&&&&&&&&&&&&&&.";
    ".&&&&&&&&&&&&&&&+&$%&$$@&$@&&$@&&&&.";
    ".&&&&&&&&&&&&&&&+&&#@%#+@#@*$%$+$&&.";
    ".&&&&&&&&&&&&&&&+&&#@#@&&@*#$%$+&&&.";
    ".&&&&&&&&&&&&&&&+&$+&*@&#@@#&#@$&&&.";
    ".&&&&&&&&&&&&&&$%@@&&*@&@#@#$#*#&&&.";
    ".&&&&&&&&&&&&&&&&&&&&&&&&&&&$%&%$&&.";
    ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
    ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
    ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
    "...................................." |]

let drag_icon =
  GdkPixbuf.from_xpm_data drag_icon_xpm
