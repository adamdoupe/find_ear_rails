class Admin::EmailBlastsController < Admin::BaseController
  def no_collection
    flash[:error] = t('challenge.no_collection', :default => "What collection did you want to work with?")
    redirect_to(request.env["HTTP_REFERER"] || root_path)
    false
  end

  def load_collection
    @collection ||= Collection.find_by_name(params[:collection_id]) if params[:collection_id]
    no_collection and return unless @collection
  end


end
