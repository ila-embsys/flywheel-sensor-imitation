with Interfaces.C; use Interfaces.C;

package Target_Speed is
   -- pragma Preelaborate;
   pragma Elaborate_Body;

   type RPM is range 100 .. 8000;
   type ADC_Measure is mod 2 ** 12;
   type Tooth is range 1 .. 88;

   Current_Angular_Velocity : RPM;
   Next_Tooth_Time : Float;
   Tooth_Counter : Tooth;

   procedure Update_RPM(
            adc_value : in int;
            next_tooth_time : out Float
         )
      with
         Export        => True,
         Convention    => C,
         External_Name => "update_rpm";

   function Calculate_Tooth_Time (current_speed : RPM) return Float;

   procedure Time_To_Tooth
      with
         Export        => True,
         Convention    => C,
         External_Name => "time_to_tooth";

   procedure Set_Pin (state : int)
     with
       Import        => True,
       Convention    => C,
       External_Name => "set_pin";

   procedure Increment_Tooth;

private
end Target_Speed;
