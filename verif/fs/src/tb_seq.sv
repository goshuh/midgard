`include "tb.svh"


typedef class tb_env;


class tb_seq extends tb_base;

   `register(tb_seq);

    tb_env  m_env;
    tb_gen  m_gen;

    mailbox m_box;

    function new();
        m_mod = "seq";

        m_gen =  tb_gen::get_inst();

       `get(tb_env,  "env", m_env);
       `get(mailbox, "box", m_box);
    endfunction

    task put(ref tb_req req);
        m_box.put(req);
    endtask

    virtual task main(input int stg, int max);
        verif::plusargs arg = new("seq.");

        string str = arg.get_str("req", "tb_req");
        int    num = arg.get_int("num",  1000);

        tb_req tbr [$];

        forever begin
            m_env.blk(stg);

            // create vma table now. simply too hard to rearrange it on-the-fly
            m_gen.init();
            m_gen.gen_all(num);

            tbr.delete();

            for (int i = 0; i < num; i++) begin
                tb_req req;

               `spawn(str, req);

                req.init(1'b1);
                req.body();
                req.post();

                tbr.push_back(req);
            end

            foreach (tbr[i])
                put(tbr[i]);

            m_env.add(1);

            m_env.blk(1 + stg + max);
            m_env.set(0);
        end
    endtask

endclass