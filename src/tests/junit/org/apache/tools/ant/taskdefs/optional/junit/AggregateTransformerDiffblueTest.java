package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.XSLTProcess;
import org.apache.tools.ant.taskdefs.XSLTProcess.Factory;
import org.apache.tools.ant.taskdefs.XSLTProcess.Factory.Feature;
import org.apache.tools.ant.taskdefs.optional.junit.AggregateTransformer.Format;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.resources.URLResource;
import org.junit.Test;
import org.w3c.dom.Document;

public class AggregateTransformerDiffblueTest {
  /**
   * Test Format {@link Format#getValues()}.
   * <p>
   * Method under test: {@link Format#getValues()}
   */
  @Test
  public void testFormatGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{AggregateTransformer.FRAMES, AggregateTransformer.NOFRAMES},
        (new Format()).getValues());
  }

  /**
   * Test Format new {@link Format} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Format}
   */
  @Test
  public void testFormatNewFormat() {
    // Arrange and Act
    Format actualFormat = new Format();

    // Assert
    assertNull(actualFormat.getValue());
    assertEquals(-1, actualFormat.getIndex());
  }

  /**
   * Test {@link AggregateTransformer#AggregateTransformer(Task)}.
   * <ul>
   *   <li>When {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then {@link AggregateTransformer#task} return {@link TaskAdapter}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AggregateTransformer#AggregateTransformer(Task)}
   */
  @Test
  public void testNewAggregateTransformer_whenTaskAdapter_thenTaskReturnTaskAdapter() throws IOException {
    // Arrange and Act
    AggregateTransformer actualAggregateTransformer = new AggregateTransformer(new TaskAdapter());

    // Assert
    assertTrue(actualAggregateTransformer.task instanceof TaskAdapter);
    assertTrue(actualAggregateTransformer.getStylesheet() instanceof URLResource);
    assertNull(actualAggregateTransformer.styleDir);
    assertNull(actualAggregateTransformer.toDir);
    assertNull(actualAggregateTransformer.document);
    String expectedStylesheetSystemId = String.join("", "file:",
        Paths
            .get(System.getProperty("user.dir"), "build", "classes", "org", "apache", "tools", "ant", "taskdefs",
                "optional", "junit", "xsl", "junit-frames.xsl")
            .toString());
    assertEquals(expectedStylesheetSystemId, actualAggregateTransformer.getStylesheetSystemId());
    assertEquals(AggregateTransformer.FRAMES, actualAggregateTransformer.format);
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link AggregateTransformer#setStyledir(File)}
   *   <li>{@link AggregateTransformer#setTodir(File)}
   *   <li>{@link AggregateTransformer#setXmlDocument(Document)}
   *   <li>{@link AggregateTransformer#getDocumentBuilderFactory()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    AggregateTransformer aggregateTransformer = new AggregateTransformer(new TaskAdapter());

    // Act
    aggregateTransformer.setStyledir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    aggregateTransformer.setTodir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    aggregateTransformer.setXmlDocument(null);

    // Assert
    assertSame(aggregateTransformer.dbfactory, aggregateTransformer.getDocumentBuilderFactory());
  }

  /**
   * Test {@link AggregateTransformer#setFormat(Format)}.
   * <ul>
   *   <li>Then {@link AggregateTransformer#AggregateTransformer(Task)} with task is {@link TaskAdapter#TaskAdapter()} {@link AggregateTransformer#format} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AggregateTransformer#setFormat(Format)}
   */
  @Test
  public void testSetFormat_thenAggregateTransformerWithTaskIsTaskAdapterFormatIsNull() {
    // Arrange
    AggregateTransformer aggregateTransformer = new AggregateTransformer(new TaskAdapter());

    // Act
    aggregateTransformer.setFormat(new Format());

    // Assert
    assertNull(aggregateTransformer.format);
  }

  /**
   * Test {@link AggregateTransformer#setXmlfile(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code Error while parsing document:} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link AggregateTransformer#setXmlfile(File)}
   */
  @Test
  public void testSetXmlfile_whenPropertyIsJavaIoTmpdirIsErrorWhileParsingDocumentToFile() throws BuildException {
    // Arrange
    AggregateTransformer aggregateTransformer = new AggregateTransformer(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> aggregateTransformer
        .setXmlfile(Paths.get(System.getProperty("java.io.tmpdir"), "Error while parsing document: ").toFile()));
  }

  /**
   * Test {@link AggregateTransformer#setXmlfile(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link AggregateTransformer#setXmlfile(File)}
   */
  @Test
  public void testSetXmlfile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws BuildException {
    // Arrange
    AggregateTransformer aggregateTransformer = new AggregateTransformer(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> aggregateTransformer.setXmlfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link AggregateTransformer#createParam()}.
   * <p>
   * Method under test: {@link AggregateTransformer#createParam()}
   */
  @Test
  public void testCreateParam() {
    // Arrange, Act and Assert
    assertNull((new AggregateTransformer(new TaskAdapter())).createParam().getType());
  }

  /**
   * Test {@link AggregateTransformer#createClasspath()}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AggregateTransformer#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenTaskAdapterProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    Project project = new Project();
    task.setProject(project);

    // Act and Assert
    assertSame(project, (new AggregateTransformer(task)).createClasspath().getProject());
  }

  /**
   * Test {@link AggregateTransformer#createClasspath()}.
   * <ul>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AggregateTransformer#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new AggregateTransformer(new TaskAdapter())).createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link AggregateTransformer#createFactory()}.
   * <p>
   * Method under test: {@link AggregateTransformer#createFactory()}
   */
  @Test
  public void testCreateFactory() {
    // Arrange and Act
    Factory actualCreateFactoryResult = (new AggregateTransformer(new TaskAdapter())).createFactory();

    // Assert
    Iterable<Feature> features = actualCreateFactoryResult.getFeatures();
    assertTrue(features instanceof List);
    assertNull(actualCreateFactoryResult.getName());
    assertTrue(((List<Feature>) features).isEmpty());
  }

  /**
   * Test {@link AggregateTransformer#checkOptions()}.
   * <p>
   * Method under test: {@link AggregateTransformer#checkOptions()}
   */
  @Test
  public void testCheckOptions() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());
    AggregateTransformer aggregateTransformer = new AggregateTransformer(task);

    // Act
    aggregateTransformer.checkOptions();

    // Assert
    File file = aggregateTransformer.toDir;
    assertEquals("apache-ant-1.10.15", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link AggregateTransformer#checkOptions()}.
   * <ul>
   *   <li>Then {@link AggregateTransformer#AggregateTransformer(Task)} with task is {@link TaskAdapter#TaskAdapter()} {@link AggregateTransformer#toDir} Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AggregateTransformer#checkOptions()}
   */
  @Test
  public void testCheckOptions_thenAggregateTransformerWithTaskIsTaskAdapterToDirNameIsTestTxt() throws BuildException {
    // Arrange
    AggregateTransformer aggregateTransformer = new AggregateTransformer(new TaskAdapter());
    aggregateTransformer.setTodir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    aggregateTransformer.checkOptions();

    // Assert that nothing has changed
    File file = aggregateTransformer.toDir;
    assertEquals("test.txt", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link AggregateTransformer#getStylesheetSystemId()}.
   * <p>
   * Method under test: {@link AggregateTransformer#getStylesheetSystemId()}
   */
  @Test
  public void testGetStylesheetSystemId() throws IOException {
    // Arrange and Act
    String actualStylesheetSystemId = (new AggregateTransformer(new TaskAdapter())).getStylesheetSystemId();

    // Assert
    assertEquals(String.join("", "file:",
        Paths
            .get(System.getProperty("user.dir"), "build", "classes", "org", "apache", "tools", "ant", "taskdefs",
                "optional", "junit", "xsl", "junit-frames.xsl")
            .toString()),
        actualStylesheetSystemId);
  }
}
