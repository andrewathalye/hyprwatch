pragma Restrictions (No_Elaboration_Code);

with D_Bus.Connection.Dispatch;

with Callback;

procedure hyprlisten is
   Bus : D_Bus.Connection.Connection_Type;
begin
   Bus := D_Bus.Connection.Connect;
   D_Bus.Connection.Add_Match
     (Bus,
      "type=signal,sender=tk.zenithseeker.hyprwatch," &
      "interface=tk.zenithseeker.hyprwatch,member=HyprUpdate," &
      "path=/tk/zenithseeker/hyprwatch");

   D_Bus.Connection.Dispatch (Connection => Bus, Callback => Callback'Access);
end hyprlisten;
