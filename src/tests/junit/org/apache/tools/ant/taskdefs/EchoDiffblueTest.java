package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.Echo.EchoLevel;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class EchoDiffblueTest {
  /**
   * Test EchoLevel new {@link EchoLevel} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link EchoLevel}
   */
  @Test
  public void testEchoLevelNewEchoLevel() {
    // Arrange and Act
    EchoLevel actualEchoLevel = new EchoLevel();

    // Assert
    assertNull(actualEchoLevel.getValue());
    assertEquals(-1, actualEchoLevel.getIndex());
  }

  /**
   * Test {@link Echo#setMessage(String)}.
   * <ul>
   *   <li>When {@code Msg}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#message} is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setMessage(String)}
   */
  @Test
  public void testSetMessage_whenMsg_thenEchoMessageIsMsg() {
    // Arrange
    Echo echo = new Echo();

    // Act
    echo.setMessage("Msg");

    // Assert
    assertEquals("Msg", echo.message);
  }

  /**
   * Test {@link Echo#setMessage(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#message} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setMessage(String)}
   */
  @Test
  public void testSetMessage_whenNull_thenEchoMessageIsEmptyString() {
    // Arrange
    Echo echo = new Echo();

    // Act
    echo.setMessage(null);

    // Assert that nothing has changed
    assertEquals("", echo.message);
  }

  /**
   * Test {@link Echo#setFile(File)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#file} Name is {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setFile(File)}
   */
  @Test
  public void testSetFile_whenNull_file_placeholder_thenEchoFileNameIsNullFile() {
    // Arrange
    Echo echo = new Echo();

    // Act
    echo.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    File file = echo.file;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Echo#setOutput(Resource)}.
   * <ul>
   *   <li>Given {@link Manifest#ATTRIBUTE_NAME}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#file} Name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setOutput(Resource)}
   */
  @Test
  public void testSetOutput_givenAttribute_name_thenEchoFileNameIsAttribute_name() {
    // Arrange
    Echo echo = new Echo();

    FileResource output = new FileResource();
    output.setName(Manifest.ATTRIBUTE_NAME);

    // Act
    echo.setOutput(output);

    // Assert
    File file = echo.file;
    assertTrue(file.isAbsolute());
    assertEquals(Manifest.ATTRIBUTE_NAME, file.getName());
    File expectedFile = echo.file;
    assertSame(expectedFile, output.getFile());
  }

  /**
   * Test {@link Echo#setOutput(Resource)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#file} Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setOutput(Resource)}
   */
  @Test
  public void testSetOutput_givenDotDot_thenEchoFileNameIsDownloads() {
    // Arrange
    Echo echo = new Echo();

    FileResource output = new FileResource();
    output.setName("..");

    // Act
    echo.setOutput(output);

    // Assert
    File file = echo.file;
    assertEquals("Downloads", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = echo.file;
    assertSame(expectedFile, output.getFile());
  }

  /**
   * Test {@link Echo#setOutput(Resource)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#file} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setOutput(Resource)}
   */
  @Test
  public void testSetOutput_givenDotDot_thenEchoFileNameIsEmptyString() {
    // Arrange
    Echo echo = new Echo();

    FileResource output = new FileResource();
    output.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    output.setName("..");

    // Act
    echo.setOutput(output);

    // Assert
    File file = echo.file;
    assertEquals("", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = echo.file;
    assertSame(expectedFile, output.getFile());
  }

  /**
   * Test {@link Echo#setOutput(Resource)}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#file} Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setOutput(Resource)}
   */
  @Test
  public void testSetOutput_givenDot_thenEchoFileNameIsApacheAnt11015() {
    // Arrange
    Echo echo = new Echo();

    FileResource output = new FileResource();
    output.setName(".");

    // Act
    echo.setOutput(output);

    // Assert
    File file = echo.file;
    assertEquals("apache-ant-1.10.15", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = echo.file;
    assertSame(expectedFile, output.getFile());
  }

  /**
   * Test {@link Echo#setOutput(Resource)}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#file} Name is {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setOutput(Resource)}
   */
  @Test
  public void testSetOutput_givenDot_whenFileResourceNameIsDot_thenEchoFileNameIsNullFile() {
    // Arrange
    Echo echo = new Echo();

    FileResource output = new FileResource();
    output.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    output.setName(".");

    // Act
    echo.setOutput(output);

    // Assert
    File file = echo.file;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = echo.file;
    assertSame(expectedFile, output.getFile());
  }

  /**
   * Test {@link Echo#setOutput(Resource)}.
   * <ul>
   *   <li>Given {@link Echo} (default constructor).</li>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then {@link FileResource#FileResource()} File is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setOutput(Resource)}
   */
  @Test
  public void testSetOutput_givenEcho_whenFileResource_thenFileResourceFileIsNull() {
    // Arrange
    Echo echo = new Echo();
    FileResource output = new FileResource();

    // Act
    echo.setOutput(output);

    // Assert that nothing has changed
    assertNull(output.getFile());
  }

  /**
   * Test {@link Echo#setOutput(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setOutput(Resource)}
   */
  @Test
  public void testSetOutput_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    Echo echo = new Echo();

    FileResource output = new FileResource();
    output.setName("");

    // Act
    echo.setOutput(output);

    // Assert
    File file = echo.file;
    assertEquals("apache-ant-1.10.15", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = echo.file;
    assertSame(expectedFile, output.getFile());
  }

  /**
   * Test {@link Echo#setOutput(Resource)}.
   * <ul>
   *   <li>Given {@code /NULL_FILE}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code /NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setOutput(Resource)}
   */
  @Test
  public void testSetOutput_givenNullFile_whenFileResourceNameIsNullFile() {
    // Arrange
    Echo echo = new Echo();

    FileResource output = new FileResource();
    output.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    output.setName("/NULL_FILE");

    // Act
    echo.setOutput(output);

    // Assert
    File file = echo.file;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = echo.file;
    assertSame(expectedFile, output.getFile());
  }

  /**
   * Test {@link Echo#setOutput(Resource)}.
   * <ul>
   *   <li>Then {@link FileResource#FileResource(File)} with f is {@link Copy#NULL_FILE_PLACEHOLDER} File is {@link Echo} (default constructor) {@link Echo#file}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#setOutput(Resource)}
   */
  @Test
  public void testSetOutput_thenFileResourceWithFIsNull_file_placeholderFileIsEchoFile() {
    // Arrange
    Echo echo = new Echo();
    FileResource output = new FileResource(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    echo.setOutput(output);

    // Assert
    File file = echo.file;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = echo.file;
    assertSame(expectedFile, output.getFile());
  }

  /**
   * Test {@link Echo#addText(String)}.
   * <ul>
   *   <li>Given {@link Echo} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#message} is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#addText(String)}
   */
  @Test
  public void testAddText_givenEchoProjectIsProject_thenEchoMessageIsMsg() {
    // Arrange
    Echo echo = new Echo();
    echo.setProject(new Project());

    // Act
    echo.addText("Msg");

    // Assert
    assertEquals("Msg", echo.message);
  }

  /**
   * Test {@link Echo#addText(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#message} is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#addText(String)}
   */
  @Test
  public void testAddText_givenJavaLangObject_thenEchoMessageIsMsg() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    Echo echo = new Echo();
    echo.setProject(project);

    // Act
    echo.addText("Msg");

    // Assert
    assertEquals("Msg", echo.message);
  }

  /**
   * Test {@link Echo#addText(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#message} is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#addText(String)}
   */
  @Test
  public void testAddText_givenProjectAddBuildListenerAntClassLoader_thenEchoMessageIsMsg() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Echo echo = new Echo();
    echo.setProject(project);

    // Act
    echo.addText("Msg");

    // Assert
    assertEquals("Msg", echo.message);
  }

  /**
   * Test {@link Echo#addText(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then {@link Echo} (default constructor) {@link Echo#message} is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Echo#addText(String)}
   */
  @Test
  public void testAddText_givenProjectAddTargetAddingReferenceAndTarget_thenEchoMessageIsMsg() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    Echo echo = new Echo();
    echo.setProject(project);

    // Act
    echo.addText("Msg");

    // Assert
    assertEquals("Msg", echo.message);
  }

  /**
   * Test new {@link Echo} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Echo}
   */
  @Test
  public void testNewEcho() {
    // Arrange and Act
    Echo actualEcho = new Echo();

    // Assert
    assertEquals("", actualEcho.message);
    assertNull(actualEcho.file);
    assertNull(actualEcho.getDescription());
    assertNull(actualEcho.getTaskName());
    assertNull(actualEcho.getTaskType());
    assertNull(actualEcho.getProject());
    assertNull(actualEcho.getOwningTarget());
    assertEquals(1, actualEcho.logLevel);
    assertFalse(actualEcho.append);
  }
}
