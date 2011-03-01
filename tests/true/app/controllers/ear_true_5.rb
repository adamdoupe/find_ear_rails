class HomeController < ApplicationController
  def edit
    @title = "Edit user info"
    @user = session[:user]
     if @user==nil
       true
    end
    if param_posted?(:user)
      attribute = params[:attribute]
      case attribute
        when "details"
          if @user.update_attributes( params[:user] )
            flash[:notice] = "User details updated."
            redirect_to :action => "index"
          end
        when "password"
          if @user.update_password(params[:user])
            flash[:notice] = "User password updated."
            redirect_to :action => "index"
          else
            flash[:notice] = "Unable to update password."
          end
      end
      params[:user].delete "password"
      params[:user].delete "current_password"
      params[:user].delete "password_confirmation"
    end
# For security purposes, never fill in password fields.
    @user.clear_password!

  end
end
