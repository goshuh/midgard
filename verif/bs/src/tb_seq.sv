`include "tb.svh"


typedef class tb_env;


class tb_seq extends tb_base;

   `register(tb_seq);

    tb_env  m_env;

    mailbox m_box;

    function new();
        m_mod = "seq";

       `get(tb_env,  "env", m_env);
       `get(mailbox, "box", m_box);
    endfunction

    task put(ref tb_req req);
        m_box.put(req);
    endtask

    virtual task main();
        verif::plusargs arg = new("seq.");

        string str = arg.get_str("req", "tb_req");
        int    num = arg.get_int("num",  1000);

        // cache is everywhere. make things simple
        tb_req tbr [$];

        forever begin
            // from tb_ctl
            m_env.blk(2);

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

            // to tb_llc
            m_env.set(3);
        end
    endtask

endclass