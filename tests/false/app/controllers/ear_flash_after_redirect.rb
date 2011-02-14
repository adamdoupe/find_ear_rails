class TopicsController < ApplicationController
  
  def update
    redirect_to redirect_path_on_access_denied(current_user)
    flash[:notice] = "Static String."
  end
  
end
