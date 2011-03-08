class UsersController < ApplicationController # ActiveRbac::ComponentController
  def also_should_be_an_error
    something()
      begin
        do_something_here()
        what_about_now("adam")
        redirect_to("/")
      rescue Exception
        this_is_when_bad_happens()
      ensure
        @user.update_attributes(params)
      end
  end
end
