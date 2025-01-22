package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.Concat.TextElement;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class ConcatDiffblueTest {
  /**
   * Test new {@link Concat} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Concat}
   */
  @Test
  public void testNewConcat() {
    // Arrange and Act
    Concat actualConcat = new Concat();

    // Assert
    assertNull(actualConcat.getDescription());
    assertNull(actualConcat.getTaskName());
    assertNull(actualConcat.getTaskType());
    assertNull(actualConcat.getProject());
    assertNull(actualConcat.getOwningTarget());
    assertEquals(1, actualConcat.size());
    assertFalse(actualConcat.isFilesystemOnly());
    assertFalse(actualConcat.isEmpty());
  }

  /**
   * Test {@link Concat#createPath()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#createPath()}
   */
  @Test
  public void testCreatePath_givenConcat() {
    // Arrange and Act
    Path actualCreatePathResult = (new Concat()).createPath();

    // Assert
    Location location = actualCreatePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePathResult.getDescription());
    assertNull(actualCreatePathResult.getProject());
    assertNull(actualCreatePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreatePathResult.size());
    assertFalse(actualCreatePathResult.isReference());
    assertTrue(actualCreatePathResult.isEmpty());
  }

  /**
   * Test {@link Concat#createPath()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#createPath()}
   */
  @Test
  public void testCreatePath_givenConcatAddFilesetFileSet() {
    // Arrange
    Concat concat = new Concat();
    concat.addFileset(new FileSet());

    // Act
    Path actualCreatePathResult = concat.createPath();

    // Assert
    Location location = actualCreatePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePathResult.getDescription());
    assertNull(actualCreatePathResult.getProject());
    assertNull(actualCreatePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreatePathResult.size());
    assertFalse(actualCreatePathResult.isReference());
    assertTrue(actualCreatePathResult.isEmpty());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Concat#setAppend(boolean)}
   *   <li>{@link Concat#setBinary(boolean)}
   *   <li>{@link Concat#setDest(Resource)}
   *   <li>{@link Concat#setFilterBeforeConcat(boolean)}
   *   <li>{@link Concat#setFixLastLine(boolean)}
   *   <li>{@link Concat#setForce(boolean)}
   *   <li>{@link Concat#setForceReadOnly(boolean)}
   *   <li>{@link Concat#setIgnoreEmpty(boolean)}
   *   <li>{@link Concat#setOutputEncoding(String)}
   *   <li>{@link Concat#setResourceName(String)}
   *   <li>{@link Concat#setWriter(Writer)}
   *   <li>{@link Concat#addFooter(TextElement)}
   *   <li>{@link Concat#addHeader(TextElement)}
   *   <li>{@link Concat#isFilesystemOnly()}
   *   <li>{@link Concat#size()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Concat concat = new Concat();

    // Act
    concat.setAppend(true);
    concat.setBinary(true);
    concat.setDest(new Resource());
    concat.setFilterBeforeConcat(true);
    concat.setFixLastLine(true);
    concat.setForce(true);
    concat.setForceReadOnly(true);
    concat.setIgnoreEmpty(true);
    concat.setOutputEncoding(Manifest.JAR_ENCODING);
    concat.setResourceName("Resource Name");
    concat.setWriter(new StringWriter());
    concat.addFooter(new TextElement());
    concat.addHeader(new TextElement());
    boolean actualIsFilesystemOnlyResult = concat.isFilesystemOnly();

    // Assert
    assertEquals(1, concat.size());
    assertFalse(actualIsFilesystemOnlyResult);
  }

  /**
   * Test {@link Concat#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) add {@link Concat} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#execute()}
   */
  @Test
  public void testExecute_givenConcatAddConcat_thenThrowBuildException() {
    // Arrange
    Concat concat = new Concat();
    concat.add(new Concat());
    concat.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> concat.execute());
  }

  /**
   * Test {@link Concat#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code Text}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#execute()}
   */
  @Test
  public void testExecute_givenConcatAddTextText_thenThrowBuildException() {
    // Arrange
    Concat concat = new Concat();
    concat.addText("Text");
    concat.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> concat.execute());
  }

  /**
   * Test {@link Concat#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Binary is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#execute()}
   */
  @Test
  public void testExecute_givenConcatBinaryIsTrue_thenThrowBuildException() {
    // Arrange
    Concat concat = new Concat();
    concat.setBinary(true);
    concat.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> concat.execute());
  }

  /**
   * Test {@link Concat#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Binary is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#execute()}
   */
  @Test
  public void testExecute_givenConcatBinaryIsTrue_thenThrowBuildException2() {
    // Arrange
    Concat concat = new Concat();
    concat.setBinary(true);
    concat.addText("Text");
    concat.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> concat.execute());
  }

  /**
   * Test {@link Concat#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#execute()}
   */
  @Test
  public void testExecute_givenConcat_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Concat()).execute());
  }

  /**
   * Test {@link Concat#execute()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#execute()}
   */
  @Test
  public void testExecute_givenFileNameNameIsDot_thenThrowBuildException() {
    // Arrange
    FileName name = new FileName();
    name.setName(".");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat concat = new Concat();
    concat.addFilelist(list);

    // Act and Assert
    assertThrows(BuildException.class, () -> concat.execute());
  }

  /**
   * Test {@link Concat#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) add {@link Concat} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#iterator()}
   */
  @Test
  public void testIterator_givenConcatAddConcat_thenThrowBuildException() {
    // Arrange
    Concat concat = new Concat();
    concat.add(new Concat());
    concat.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> concat.iterator());
  }

  /**
   * Test {@link Concat#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code Text}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#iterator()}
   */
  @Test
  public void testIterator_givenConcatAddTextText_thenThrowBuildException() {
    // Arrange
    Concat concat = new Concat();
    concat.addText("Text");
    concat.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> concat.iterator());
  }

  /**
   * Test {@link Concat#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Binary is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#iterator()}
   */
  @Test
  public void testIterator_givenConcatBinaryIsTrue_thenThrowBuildException() {
    // Arrange
    Concat concat = new Concat();
    concat.setBinary(true);
    concat.addText("Text");
    concat.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> concat.iterator());
  }

  /**
   * Test {@link Concat#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Concat#iterator()}
   */
  @Test
  public void testIterator_givenConcat_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Concat()).iterator());
  }

  /**
   * Test TextElement {@link TextElement#addText(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then {@link TextElement} (default constructor) Value is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#addText(String)}
   */
  @Test
  public void testTextElementAddText_givenJavaLangObject_thenTextElementValueIs42() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    TextElement textElement = new TextElement();
    textElement.setProject(project);

    // Act
    textElement.addText("42");

    // Assert
    assertEquals("42", textElement.getValue());
  }

  /**
   * Test TextElement {@link TextElement#addText(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#addText(String)}
   */
  @Test
  public void testTextElementAddText_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TextElement textElement = new TextElement();
    textElement.setProject(project);

    // Act
    textElement.addText("42");

    // Assert
    assertEquals("42", textElement.getValue());
  }

  /**
   * Test TextElement {@link TextElement#addText(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#addText(String)}
   */
  @Test
  public void testTextElementAddText_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    TextElement textElement = new TextElement();
    textElement.setProject(project);

    // Act
    textElement.addText("42");

    // Assert
    assertEquals("42", textElement.getValue());
  }

  /**
   * Test TextElement {@link TextElement#addText(String)}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link TextElement} (default constructor) Value is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#addText(String)}
   */
  @Test
  public void testTextElementAddText_givenTextElementProjectIsProject_thenTextElementValueIs42() {
    // Arrange
    TextElement textElement = new TextElement();
    textElement.setProject(new Project());

    // Act
    textElement.addText("42");

    // Assert
    assertEquals("42", textElement.getValue());
  }

  /**
   * Test TextElement {@link TextElement#getValue()}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#getValue()}
   */
  @Test
  public void testTextElementGetValue_givenTextElementProjectIsProject_thenReturn42() {
    // Arrange
    TextElement textElement = new TextElement();
    textElement.setProject(new Project());
    textElement.addText("42");

    // Act and Assert
    assertEquals("42", textElement.getValue());
  }

  /**
   * Test TextElement {@link TextElement#getValue()}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor) Trim is {@code true}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#getValue()}
   */
  @Test
  public void testTextElementGetValue_givenTextElementTrimIsTrue_thenReturnEmptyString() {
    // Arrange
    TextElement textElement = new TextElement();
    textElement.setTrim(true);

    // Act and Assert
    assertEquals("", textElement.getValue());
  }

  /**
   * Test TextElement {@link TextElement#getValue()}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor) TrimLeading is {@code true}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#getValue()}
   */
  @Test
  public void testTextElementGetValue_givenTextElementTrimLeadingIsTrue_thenReturn42() {
    // Arrange
    TextElement textElement = new TextElement();
    textElement.setTrimLeading(true);
    textElement.setProject(new Project());
    textElement.addText("42");

    // Act and Assert
    assertEquals("42", textElement.getValue());
  }

  /**
   * Test TextElement {@link TextElement#getValue()}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor) TrimLeading is {@code true}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#getValue()}
   */
  @Test
  public void testTextElementGetValue_givenTextElementTrimLeadingIsTrue_thenReturnEmptyString() {
    // Arrange
    TextElement textElement = new TextElement();
    textElement.setTrimLeading(true);

    // Act and Assert
    assertEquals("", textElement.getValue());
  }

  /**
   * Test TextElement {@link TextElement#getValue()}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#getValue()}
   */
  @Test
  public void testTextElementGetValue_givenTextElement_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new TextElement()).getValue());
  }

  /**
   * Test TextElement new {@link TextElement} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TextElement}
   */
  @Test
  public void testTextElementNewTextElement() {
    // Arrange and Act
    TextElement actualTextElement = new TextElement();

    // Assert
    assertEquals("", actualTextElement.getValue());
    Location location = actualTextElement.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTextElement.getDescription());
    assertNull(actualTextElement.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test TextElement {@link TextElement#setFile(File)}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#setFile(File)}
   */
  @Test
  public void testTextElementSetFile_givenTextElement() throws BuildException {
    // Arrange
    TextElement textElement = new TextElement();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> textElement.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test TextElement {@link TextElement#setFile(File)}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor) Encoding is {@code Encoding}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#setFile(File)}
   */
  @Test
  public void testTextElementSetFile_givenTextElementEncodingIsEncoding() throws BuildException {
    // Arrange
    TextElement textElement = new TextElement();
    textElement.setEncoding("Encoding");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> textElement.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test TextElement {@link TextElement#setFile(File)}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor) Encoding is {@link Manifest#JAR_ENCODING}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#setFile(File)}
   */
  @Test
  public void testTextElementSetFile_givenTextElementEncodingIsJar_encoding() throws BuildException {
    // Arrange
    TextElement textElement = new TextElement();
    textElement.setEncoding(Manifest.JAR_ENCODING);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> textElement.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test TextElement {@link TextElement#setFile(File)}.
   * <ul>
   *   <li>Given {@link TextElement} (default constructor).</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TextElement#setFile(File)}
   */
  @Test
  public void testTextElementSetFile_givenTextElement_whenNull_file_placeholder() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TextElement()).setFile(Copy.NULL_FILE_PLACEHOLDER));
  }
}
