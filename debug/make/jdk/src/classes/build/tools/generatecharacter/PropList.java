package build.tools.generatecharacter;

import java.util.regex.*;
import java.util.*;
import java.io.*;

/**
 * A PropList object contains the lists of code points that have
 * the same Unicode property defined in PropList.txt
 */
public class PropList {
    public static PropList readSpecFile(File file, int plane) throws IOException
    {
        return new PropList(file, plane);
    }

    public List<Integer> codepoints(String name) {
        return propMap.get(name);
    }

    public Set<String> names() {
        return propMap.keySet();
    }

    private Map<String, ArrayList<Integer>> propMap = new LinkedHashMap<String, ArrayList<Integer>>();

    private PropList(File file, int plane) throws IOException {
        int i, j;
        BufferedReader sbfr = new BufferedReader(new FileReader(file));
        Matcher m = Pattern.compile("(\\p{XDigit}+)(?:\\.{2}(\\p{XDigit}+))?\\s*;\\s+(\\w+)\\s+#.*").matcher("");
        String line = null;
        int lineNo = 0;
        while ((line = sbfr.readLine()) != null) {
            lineNo++;
            if (line.length() <= 1 || line.charAt(0) == '#') {
                continue;
            }
            m.reset(line);
            if (m.matches()) {
                int start = Integer.parseInt(m.group(1), 16);
                if ((start >> 16) != plane)
                    continue;
                int end = (m.group(2)==null)?start
                          :Integer.parseInt(m.group(2), 16);
                String name = m.group(3);

                start &= 0xffff;
                end &= 0xffff;

                ArrayList<Integer> list = propMap.get(name);
                if (list == null) {
                    list = new ArrayList<Integer>();
                    propMap.put(name, list);
                }
                while (start <= end)
                    list.add(start++);
            } else {
                System.out.printf("Warning: Unrecognized line %d <%s>%n", lineNo, line);
            }
        }
        sbfr.close();
    }

    public static void main(String[] args) throws IOException {
        readSpecFile(new File(args[0]), Integer.decode(args[1]));
    }
}
