class UpdatesController < ApplicationController
  
  def authorized?(action = action_name, resource = nil)
    logged_in? &&
      case action
        when 'edit', 'update'
          @update ||= Update.find params[:id]
          @update.creator == current_user ||
          redirect_to(tickets_path)
        when 'destroy'
          @update ||= Update.find params[:id]
          @update.creator == current_user ||
          redirect_to(tickets_path)
        else
          true
      end
  end
end
