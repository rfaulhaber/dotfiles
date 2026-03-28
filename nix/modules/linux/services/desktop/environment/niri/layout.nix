{colors}: {
  gaps = 16;
  center-focused-column = "never";

  preset-column-widths = [
    {proportion = 1.0 / 3.0;}
    {proportion = 1.0 / 2.0;}
    {proportion = 2.0 / 3.0;}
  ];

  default-column-width = {proportion = 0.5;};

  focus-ring = {
    enable = true;
    width = 4;
    active.color = colors.withHashtag.teal;
    inactive.color = colors.withHashtag.dark-blue;
  };

  border = {
    enable = false;
  };

  shadow = {
    enable = false;
    softness = 30;
    spread = 5;
    offset = {
      x = 0;
      y = 5;
    };
    color = colors.withHashtag.bg;
  };
}
