import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.NumberTickUnit;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.ApplicationFrame;
import org.jfree.ui.RefineryUtilities;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class ContinuousPlot extends ApplicationFrame {
    private List<XYSeries> series;
    private XYSeriesCollection dataset;
    private JFreeChart chart;
    private ChartPanel panel;
    private XYLineAndShapeRenderer rend;
    private XYPlot plot;
    private NumberAxis yAxis, xAxis;

    public ContinuousPlot(String appTitle, String chartTitle, String xLabel, String yLabel, int width, int height){
        super(appTitle);
        chart = ChartFactory.createXYLineChart(chartTitle, xLabel, yLabel,
                                                                createDataset(),
                                                                PlotOrientation.VERTICAL,
                                                                true , true , false);
        chart.getPlot().setBackgroundPaint(Color.WHITE);
        panel = new ChartPanel(chart);
        panel.setPreferredSize(new java.awt.Dimension(width , height));
        BasicStroke gridline = new BasicStroke(0.5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_BEVEL, 20, new float[]{3f}, 30);
        plot = chart.getXYPlot();
        xAxis = (NumberAxis) plot.getDomainAxis();
        yAxis = (NumberAxis) plot.getRangeAxis();
        plot.setDomainGridlinesVisible(true);
        plot.setDomainGridlinePaint(Color.GRAY);
        plot.setDomainGridlineStroke(gridline);
        plot.setRangeGridlinesVisible(true);
        plot.setRangeGridlinePaint(Color.GRAY);
        plot.setRangeGridlineStroke(gridline);
        rend = new XYLineAndShapeRenderer();
        plot.setRenderer(rend);
        setContentPane(panel);
    }
    private XYDataset createDataset(){
        series = new ArrayList<>();
        dataset = new XYSeriesCollection();
        return dataset;
    }
    public int findSeries(String key){
      for(int i = 0; i < series.size(); i++) {
          if(series.get(i).getKey().equals(key)) {
              return i;
          }
      }
      return -1;
    }
    public void addSeries(String key){
        series.add(new XYSeries(key));
        dataset.addSeries(series.get(findSeries(key)));
    }
    public void addData(String key, double x, double y){
        int i = findSeries(key);
        series.get(i).add(x,y);
    }
    public void clearData(String key){
        series.get(findSeries(key)).clear();
    }
    public void setLine(String key, Color color, float width){
        int i = findSeries(key);
        rend.setSeriesPaint(i , color);
        rend.setSeriesStroke(i , new BasicStroke(width));
        plot.setRenderer(rend);
    }
    public void setXAxis(float min, float max, float tick){
        xAxis.setRange(min, max);
        xAxis.setTickUnit(new NumberTickUnit(tick));
    }
    public void setYAxis(float min, float max, float tick){
        yAxis.setRange(min, max);
        yAxis.setTickUnit(new NumberTickUnit(tick));
    }
    public static void main(String[] args) throws InterruptedException {
        ContinuousPlot plot = new ContinuousPlot("myApp", "myChart", "xLabel", "yLabel", 1300, 700);
        plot.addSeries("sin");
        for(float i = 0.0f; i < (float) 2*Math.PI; i += 0.1) {
            plot.addData("sin", i, Math.sin(i));
        }
        plot.setLine("sin", Color.BLUE, 6.0f);
        plot.pack();
        RefineryUtilities.centerFrameOnScreen(plot);
        plot.setVisible(true);
        plot.addSeries("cos");
        plot.setLine("cos", Color.GREEN, 4.0f);
        plot.setXAxis(0.0f,(float) (2*Math.PI), 0.1f);
        plot.setYAxis(-1.0f, 3.0f, 0.1f);
        for(int j = 0; j < 3; j++) {
            for(float i = 0.0f; i < (float) 2*Math.PI; i += 0.1) {
                plot.addData("cos", i, Math.cos(i));
                Thread.sleep(30);
            }
            plot.clearData("cos");
        }

    }
}
