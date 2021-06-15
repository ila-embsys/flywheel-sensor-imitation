with Interfaces.C; use Interfaces.C;

package Target_Speed is
   -- pragma Preelaborate;
   pragma "Elaborate_Body";

   type RPM is range 0 .. 8000;

   Current_Angular_Velocity : RPM;

   procedure Update_RPM(current_rpm : int)
      with
         Export        => True,
         Convention    => C,
         External_Name => "update_rpm";
private
end Target_Speed;
