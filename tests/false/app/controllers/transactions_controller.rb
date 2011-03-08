class TransactionsController < ApplicationController
  def index
    @q = (params[:q] || "").dup
    options = {:per_page => 30, :order => "posted_on DESC, id DESC"}
    unless @q.blank? then
      conditions = []
      values = {}
      @q.gsub!(/(?:^|\s)[-\d]+(\s|$)/) do |partial_date|
        returning "" do
          conditions << "posted_on BETWEEN :start AND :end"
          case partial_date.gsub("-", "")
          when /(\d{4})(\d{2})(\d{2})/
            values[:start] = values[:end] = Date.new($1.to_i, $2.to_i, $3.to_i)
          when /(\d{4})(\d{2})/
            values[:start] = Date.new($1.to_i, $2.to_i, 1)
            values[:end] = values[:start] >> 1
          when /(\d{4})/
            values[:start] = Date.new($1.to_i, 1, 1)
            values[:end] = values[:start] >> 12
          else
            flash_failure :now, "Impossible de lire la date dans: #{partial_date.inspect}"
            conditions.pop
          end
        end
      end

      @q.strip!
      unless @q.blank? then
        conditions << "LOWER(description) LIKE :description"
        values[:description] = "%#{@q.downcase}%"
      end

      options[:conditions] = ["(#{conditions.join(") AND (")})", values] unless conditions.empty?
    end

    @q = params[:q]
    @transactions = Txn.paginate(:all, options.merge(:page => params[:page]))
  end

  def new
    @transaction = Txn.new
    self.count_lines! if request.get?
    update_and_redirect if request.post?
  end

  def edit
    @transaction = Txn.find(params[:txn_id])
    self.count_lines! if request.get?
    update_and_redirect if request.post?
  end

  def add_line
    render(:nothing => true) if params[:nline][:no].blank?
    @line = TxnAccount.new(params[:nline])
    @line_count = 1 + params[:line_count].to_i
  end

  def delete_line
    @transaction = Txn.find(params[:txn_id])
    @line = @transaction.lines.find(params[:line])
    @line.destroy
    self.count_lines!
    @index = params[:index]
  end

  protected
  def update_and_redirect
    if params[:destroy] then
      @transaction.destroy
      return redirect_to(transactions_url)
    end

    Txn.transaction do
      @transaction.attributes = params[:transaction]
      (params[:line] || []).each do |id, values|
        @line = @transaction.lines.find_by_id(id) || @transaction.lines.build
        @line.attributes = values
      end

      @transaction.save! # Final balance verification
    end

    if params[:commit] =~ /nouveau/i then
      redirect_to transaction_new_url
    else
      redirect_to transactions_url
    end

    rescue ActiveRecord::RecordInvalid
      # NOP
  end

  def count_lines!
    return @transaction.lines.count if @transaction.new_record?
    count = TxnAccount.connection.select_value("SELECT MAX(id) FROM #{TxnAccount.table_name} WHERE txn_id = #{@transaction.id}")
    @line_count = count.to_i rescue 0
  end
end
