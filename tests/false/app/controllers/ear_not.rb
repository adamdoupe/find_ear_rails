
class UsersController < ApplicationController # ActiveRbac::ComponentController

  def test_with_bang
    if !always_return_false_redirect_to
      return
    end
    @user.update_attributes(params[:user])
  end

  def test_with_not
    if not always_return_false_redirect_to
      return
    end
    @user.update_attributes(params[:user])
  end

  def test_with_bang_true
    unless !redirect_to
      return
    end
    @user.update_attributes(params[:user])
  end



end
