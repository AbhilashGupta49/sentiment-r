package formater_for_R;
import java.io.*;


// efficient program for formating amazon review dataset from kaggle

public class formater {

	public static void main(String[] args) {
		try {
		String file = "filename.txt";
		String out_file="formatted.txt";
		
		
		BufferedReader bin = new BufferedReader(new InputStreamReader(new FileInputStream(file)));		
		BufferedWriter bout = new BufferedWriter(new OutputStreamWriter( new FileOutputStream(out_file)));
		
		String line = "";
		System.out.println("Start");
		while((line=bin.readLine())!=null) {
			String[] arr = line.split(" ");
			for(int i=0;i<arr.length;i++) {
				String word = arr[i];
				if(!(word.contains("__label__"))) {
					bout.append(word+" ");
			

				}
			}
			bout.append("\n<---> \n");
		}
		
		bin.close();
		bout.close();
		System.out.println("end");
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

}
