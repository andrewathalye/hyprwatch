package Hyprland is
   pragma Pure (Hyprland);

   --  Shared constants for all Hyprland packages.
   --  See `Hyprland.Protocol` for a low-level interface
   --  and `Hyprland.State` for a high-level interface

   --------------------
   --  Enumerations  --
   --------------------

   type Hypr1_Message_Id is
     (Dispatch, Keyword, Reload, Kill, Setcursor, Output, Switchxkblayout,
      Seterror, Setprop, Notify, Dismissnotify, Version, Monitors, Workspaces,
      Activeworkspace, Workspacerules, Clients, Devices, Decorations, Binds,
      Activewindow, Layers, Splash, Getoption, Cursorpos, Animations,
      Instances, Layouts, Configerrors, Rollinglog, Locked);

   type Hypr2_Message_Id is
     (Workspace, Workspacev2, Focusedmon, Focusedmonv2, Activewindow,
      Activewindowv2, Fullscreen, Monitorremoved, Monitorremovedv2,
      Monitoradded, Monitoraddedv2, Createworkspace, Createworkspacev2,
      Destroyworkspace, Destroyworkspacev2, Moveworkspace, Moveworkspacev2,
      Renameworkspace, Activespecial, Activespecialv2, Activelayout,
      Openwindow, Closewindow, Movewindow, Movewindowv2, Openlayer,
      Closelayer, Submap, Changefloatingmode, Urgent, Screencast,
      Windowtitle, Windowtitlev2, Togglegroup, Moveintogroup, Moveoutofgroup,
      Ignoregrouplock, Lockgroups, Configreloaded, Pin, Minimized, Bell);
end Hyprland;
