pragma Ada_2012;

with Ada.Text_IO;

with Hypr_Helpers;

package body Glib_Helpers is

   --------------------------
   -- Hypr_Source_Callback --
   --------------------------
   function Hypr_Source_Callback
     (Source : Glib.IOChannel.Giochannel;
      Condition : Glib.IOChannel.GIOCondition;
      Data : in out Hyprland.State.Hyprland_State) return Boolean
   is
      pragma Unreferenced (Source);
      pragma Unreferenced (Condition);

      Has_Updates : Boolean;
   begin
      Has_Updates := Data.Update;

      if Has_Updates then
         Ada.Text_IO.Put_Line
           (Hypr_Helpers.Generate_Status_JSON (Data).Write);
      end if;

      return True;
   end Hypr_Source_Callback;

   -----------------
   -- GIO_Sources --
   -----------------
   package body GIO_Sources is
      --  Container for wrapper data
      type Wrapper_Data is record
         Func : GIO_Source_Func;
         Data : Data_Access;
         Notify : Destroy_Notify;
      end record
      with
         Convention => C;
      type Wrapper_Data_Access is access Wrapper_Data;

      type Wrapped_GIO_Source_Func is access function
        (Source : Glib.IOChannel.Giochannel;
         Condition : Glib.IOChannel.GIOCondition;
         Data : access Wrapper_Data) return Glib.Gboolean
      with
         Convention => C;

      type Wrapped_Destroy_Notify is access procedure
        (Data : access Wrapper_Data)
      with
         Convention => C;

      --  Wrap a `GIO_Source_Func` for use in C code
      function GIO_Source_Func_Wrapper
        (Source : Glib.IOChannel.Giochannel;
         Condition : Glib.IOChannel.GIOCondition;
         Data : access Wrapper_Data) return Glib.Gboolean
      with
         Convention => C;
      function GIO_Source_Func_Wrapper
        (Source : Glib.IOChannel.Giochannel;
         Condition : Glib.IOChannel.GIOCondition;
         Data : access Wrapper_Data) return Glib.Gboolean
      is
         Result : Boolean;
      begin
         Result := Data.Func (Source, Condition, Data.Data.all);
         --  Call Ada callback with the correct parameters
         return (if Result then Glib.Gboolean (1) else Glib.Gboolean (0));
      end GIO_Source_Func_Wrapper;

      --  Wrap a `Destroy_Notify` for use in C code
      procedure Destroy_Notify_Wrapper (Data : access Wrapper_Data)
      with
         Convention => C;
      procedure Destroy_Notify_Wrapper (Data : access Wrapper_Data) is
      begin
         if Data.Notify /= null then
            Data.Notify (Data.Data.all);
         end if;
      end Destroy_Notify_Wrapper;

      --  Underlying Glib C function
      --  Note that `Data` needs to be accessible for a long time,
      --  so we have to use a heap-allocated Access Type
      procedure G_Source_Set_Callback
        (Source : Glib.Main.G_Source;
         Func : Wrapped_GIO_Source_Func;
         Data : access Wrapper_Data;
         Notify : Wrapped_Destroy_Notify)
      with
         Import => True,
         Convention => C;

      --  User-facing procedure
      procedure Set_Callback (
            Source : Glib.Main.G_Source;
            Func : GIO_Source_Func;
            Data : Data_Access;
            Notify : Destroy_Notify := null)
      is
      begin
         G_Source_Set_Callback
           (Source => Source,
            Func => GIO_Source_Func_Wrapper'Access,
            Data =>
               Wrapper_Data_Access'(new Wrapper_Data'(Func, Data, Notify)),
            Notify => Destroy_Notify_Wrapper'Access);
      end Set_Callback;
   end GIO_Sources;
end Glib_Helpers;
