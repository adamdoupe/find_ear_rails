class ApplicationController < ActionController::Base
  protect_from_forgery
  filter_parameter_logging :password, :password_confirmation
  helper_method :current_user_session, :current_user, :root_content_node


  def always_return_false_redirect_to
    if something() 
      redirect_to '/home'
      return false
    end
    return true
  end

  def always_return_false_same_as_redirect
    if something() 
      same_as_redirect
      return false
    end
    return true
  end

  def same_as_redirect
    redirect_to(session[:return_to] || default)
    session[:return_to] = nil
  end

  def redirect_with_flash(message, destination_path)
    flash[:notice] = message
    redirect_to destination_path and return
  end




end
 
