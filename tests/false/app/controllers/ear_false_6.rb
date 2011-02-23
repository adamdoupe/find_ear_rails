class UpdatesController < ApplicationController

  def check_user
    redirect_with_flash("You may not delete this user. It is you!", admin_users_url) and return if current_user == @user
    redirect_with_flash("You may not delete this user. It is the last one!") and return if User.all.length <= 1
  end

end
