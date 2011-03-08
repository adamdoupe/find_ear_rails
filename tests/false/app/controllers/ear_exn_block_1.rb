class UsersController < ApplicationController # ActiveRbac::ComponentController
  
  def test
    begin
      redirect_to "/"
    rescue
      something()
    end
  end

  somethingelse()

  def after
    something_else()
    @users.update_attributes(@params)
  end
end
