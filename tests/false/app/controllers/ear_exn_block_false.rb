class UsersController < ApplicationController # ActiveRbac::ComponentController
  
  def test
    begin
      unless testing()
        redirect_to "/" and return
      end
      hello_world()
    rescue
      something()
    ensure
      nil
    end
  end


end
