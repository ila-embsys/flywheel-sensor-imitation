with Target_Speed;
with Interfaces.C; use Interfaces.C;

package body Target_Speed is
    procedure Update_RPM (current_rpm : int) is
    begin
        Current_Angular_Velocity := RPM(Integer(current_rpm));
    end Update_RPM;

begin
    Current_Angular_Velocity := 0;
    
end Target_Speed;
