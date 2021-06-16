with Interfaces.C; use Interfaces.C;

package Target_Speed is
   -- pragma Preelaborate;
   pragma Elaborate_Body;

   type RPM is range 900 .. 8000;
   type ADC_Measure is mod 2 ** 12;

   Current_Angular_Velocity : RPM;

   procedure Update_RPM(adc_value : int)
      with
         Export        => True,
         Convention    => C,
         External_Name => "update_rpm";
private
end Target_Speed;
