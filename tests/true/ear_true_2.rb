=begin

PersonContoller
================================

A controller which handles a single user. For processing collections of users,
see PeopleController.

=end

class PersonController < ApplicationController

  def search
    redirect_to :controller => 'people' unless @user
    if request.post?
      path = parse_filter_path(params[:search])
      redirect_to url_for_user(@user, :action => 'search', :path => path)
    else
      @path.default_sort('updated_at').merge!(:contributed => @user.id)
      @pages = Page.paginate_by_path(@path, options_for_user(@user, pagination_params))
    end

    handle_rss :title => @user.name, :link => url_for_user(@user),
      :image => avatar_url_for(@user, 'xlarge')
  end

end
