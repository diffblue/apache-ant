package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.resources.URLResource;
import org.junit.Test;

public class XMLResultAggregatorDiffblueTest {
  /**
   * Test {@link XMLResultAggregator#createReport()}.
   * <p>
   * Method under test: {@link XMLResultAggregator#createReport()}
   */
  @Test
  public void testCreateReport() throws IOException {
    // Arrange
    XMLResultAggregator xmlResultAggregator = new XMLResultAggregator();

    // Act
    AggregateTransformer actualCreateReportResult = xmlResultAggregator.createReport();

    // Assert
    assertTrue(actualCreateReportResult.task instanceof XMLResultAggregator);
    assertTrue(actualCreateReportResult.getStylesheet() instanceof URLResource);
    assertNull(actualCreateReportResult.styleDir);
    assertNull(actualCreateReportResult.toDir);
    assertNull(actualCreateReportResult.document);
    assertEquals(1, xmlResultAggregator.transformers.size());
    String expectedStylesheetSystemId = String.join("", "file:",
        Paths
            .get(System.getProperty("user.dir"), "build", "classes", "org", "apache", "tools", "ant", "taskdefs",
                "optional", "junit", "xsl", "junit-frames.xsl")
            .toString());
    assertEquals(expectedStylesheetSystemId, actualCreateReportResult.getStylesheetSystemId());
    assertEquals(AggregateTransformer.FRAMES, actualCreateReportResult.format);
  }

  /**
   * Test {@link XMLResultAggregator#addFileSet(FileSet)}.
   * <p>
   * Method under test: {@link XMLResultAggregator#addFileSet(FileSet)}
   */
  @Test
  public void testAddFileSet() {
    // Arrange
    XMLResultAggregator xmlResultAggregator = new XMLResultAggregator();
    FileSet fs = new FileSet();

    // Act
    xmlResultAggregator.addFileSet(fs);

    // Assert
    Vector<FileSet> fileSetList = xmlResultAggregator.filesets;
    assertEquals(1, fileSetList.size());
    assertSame(fs, fileSetList.get(0));
  }

  /**
   * Test {@link XMLResultAggregator#getDestinationFile()}.
   * <ul>
   *   <li>Given {@link XMLResultAggregator} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLResultAggregator#getDestinationFile()}
   */
  @Test
  public void testGetDestinationFile_givenXMLResultAggregatorProjectIsProject() {
    // Arrange
    XMLResultAggregator xmlResultAggregator = new XMLResultAggregator();
    xmlResultAggregator.setProject(new Project());

    // Act
    File actualDestinationFile = xmlResultAggregator.getDestinationFile();

    // Assert
    assertTrue(actualDestinationFile.isAbsolute());
    assertEquals(XMLResultAggregator.DEFAULT_FILENAME, actualDestinationFile.getName());
  }

  /**
   * Test {@link XMLResultAggregator#getDestinationFile()}.
   * <ul>
   *   <li>Given {@link XMLResultAggregator} (default constructor) Tofile is {@code null}.</li>
   *   <li>Then return Absolute.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLResultAggregator#getDestinationFile()}
   */
  @Test
  public void testGetDestinationFile_givenXMLResultAggregatorTofileIsNull_thenReturnAbsolute() {
    // Arrange
    XMLResultAggregator xmlResultAggregator = new XMLResultAggregator();
    xmlResultAggregator.setTofile(null);
    xmlResultAggregator.setTodir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    File actualDestinationFile = xmlResultAggregator.getDestinationFile();

    // Assert
    assertTrue(actualDestinationFile.isAbsolute());
    assertEquals(XMLResultAggregator.DEFAULT_FILENAME, actualDestinationFile.getName());
  }

  /**
   * Test {@link XMLResultAggregator#getFiles()}.
   * <ul>
   *   <li>Given {@link XMLResultAggregator} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLResultAggregator#getFiles()}
   */
  @Test
  public void testGetFiles_givenXMLResultAggregator_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new XMLResultAggregator()).getFiles().length);
  }

  /**
   * Test new {@link XMLResultAggregator} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link XMLResultAggregator}
   */
  @Test
  public void testNewXMLResultAggregator() {
    // Arrange and Act
    XMLResultAggregator actualXmlResultAggregator = new XMLResultAggregator();

    // Assert
    assertNull(actualXmlResultAggregator.toDir);
    assertNull(actualXmlResultAggregator.getDescription());
    assertNull(actualXmlResultAggregator.getTaskName());
    assertNull(actualXmlResultAggregator.getTaskType());
    assertNull(actualXmlResultAggregator.getProject());
    assertNull(actualXmlResultAggregator.getOwningTarget());
    assertEquals(0, actualXmlResultAggregator.getFiles().length);
    assertEquals(0, actualXmlResultAggregator.generatedId);
    assertTrue(actualXmlResultAggregator.filesets.isEmpty());
    assertTrue(actualXmlResultAggregator.transformers.isEmpty());
  }
}
