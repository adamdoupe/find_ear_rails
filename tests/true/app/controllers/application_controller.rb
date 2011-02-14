class ApplicationController < ActionController::Base
  protect_from_forgery
  filter_parameter_logging :password, :password_confirmation
  helper_method :current_user_session, :current_user, :root_content_node


  def second_order
    redirect_back_or_default("something")
  end

  def redirect_back_or_default(default)
    redirect_to(session[:return_to] || default)
    session[:return_to] = nil
  end


end
