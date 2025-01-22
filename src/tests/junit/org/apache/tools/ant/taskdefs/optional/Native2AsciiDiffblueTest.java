package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.native2ascii.BuiltinNative2Ascii;
import org.apache.tools.ant.taskdefs.optional.native2ascii.Native2AsciiAdapter;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.facade.ImplementationSpecificArgument;
import org.junit.Test;

public class Native2AsciiDiffblueTest {
  /**
   * Test new {@link Native2Ascii} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Native2Ascii}
   */
  @Test
  public void testNewNative2Ascii() {
    // Arrange and Act
    Native2Ascii actualNative2Ascii = new Native2Ascii();

    // Assert
    assertNull(actualNative2Ascii.getDescription());
    assertNull(actualNative2Ascii.getTaskName());
    assertNull(actualNative2Ascii.getTaskType());
    assertNull(actualNative2Ascii.getEncoding());
    assertNull(actualNative2Ascii.getProject());
    assertNull(actualNative2Ascii.getOwningTarget());
    assertEquals(0, actualNative2Ascii.getCurrentArgs().length);
    assertFalse(actualNative2Ascii.hasSelectors());
    assertFalse(actualNative2Ascii.getReverse());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Native2Ascii#setDest(File)}
   *   <li>{@link Native2Ascii#setEncoding(String)}
   *   <li>{@link Native2Ascii#setExt(String)}
   *   <li>{@link Native2Ascii#setReverse(boolean)}
   *   <li>{@link Native2Ascii#setSrc(File)}
   *   <li>{@link Native2Ascii#getEncoding()}
   *   <li>{@link Native2Ascii#getReverse()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Native2Ascii native2Ascii = new Native2Ascii();

    // Act
    native2Ascii.setDest(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    native2Ascii.setEncoding("UTF-8");
    native2Ascii.setExt("Ext");
    native2Ascii.setReverse(true);
    native2Ascii.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    String actualEncoding = native2Ascii.getEncoding();

    // Assert
    assertEquals("UTF-8", actualEncoding);
    assertTrue(native2Ascii.getReverse());
  }

  /**
   * Test {@link Native2Ascii#createMapper()}.
   * <ul>
   *   <li>Given {@link Native2Ascii} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2Ascii#createMapper()}
   */
  @Test
  public void testCreateMapper_givenNative2AsciiAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    Native2Ascii native2Ascii = new Native2Ascii();
    native2Ascii.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> native2Ascii.createMapper());
  }

  /**
   * Test {@link Native2Ascii#createMapper()}.
   * <ul>
   *   <li>Given {@link Native2Ascii} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2Ascii#createMapper()}
   */
  @Test
  public void testCreateMapper_givenNative2Ascii_thenReturnLocationFileNameIsNull() throws BuildException {
    // Arrange and Act
    Mapper actualCreateMapperResult = (new Native2Ascii()).createMapper();

    // Assert
    Location location = actualCreateMapperResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateMapperResult.getDescription());
    assertNull(actualCreateMapperResult.getProject());
    assertNull(actualCreateMapperResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreateMapperResult.isReference());
  }

  /**
   * Test {@link Native2Ascii#add(Native2AsciiAdapter)} with {@code adapter}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2Ascii#add(Native2AsciiAdapter)}
   */
  @Test
  public void testAddWithAdapter_thenThrowBuildException() {
    // Arrange
    Native2Ascii native2Ascii = new Native2Ascii();
    native2Ascii.add(new BuiltinNative2Ascii());

    // Act and Assert
    assertThrows(BuildException.class, () -> native2Ascii.add(new BuiltinNative2Ascii()));
  }

  /**
   * Test {@link Native2Ascii#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2Ascii#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_thenThrowBuildException() {
    // Arrange
    Native2Ascii native2Ascii = new Native2Ascii();
    native2Ascii.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> native2Ascii.add(new CutDirsMapper()));
  }

  /**
   * Test {@link Native2Ascii#execute()}.
   * <p>
   * Method under test: {@link Native2Ascii#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Native2Ascii native2Ascii = new Native2Ascii();
    native2Ascii.setDest(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    native2Ascii.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> native2Ascii.execute());
  }

  /**
   * Test {@link Native2Ascii#execute()}.
   * <ul>
   *   <li>Given {@link Native2Ascii} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2Ascii#execute()}
   */
  @Test
  public void testExecute_givenNative2AsciiProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Native2Ascii native2Ascii = new Native2Ascii();
    native2Ascii.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> native2Ascii.execute());
  }

  /**
   * Test {@link Native2Ascii#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2Ascii#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    Native2Ascii native2Ascii = new Native2Ascii();
    native2Ascii.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> native2Ascii.execute());
  }

  /**
   * Test {@link Native2Ascii#getCurrentArgs()}.
   * <ul>
   *   <li>Given {@link Native2Ascii} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2Ascii#getCurrentArgs()}
   */
  @Test
  public void testGetCurrentArgs_givenNative2Ascii() {
    // Arrange, Act and Assert
    assertEquals(0, (new Native2Ascii()).getCurrentArgs().length);
  }

  /**
   * Test {@link Native2Ascii#getCurrentArgs()}.
   * <ul>
   *   <li>Given {@link Native2Ascii} (default constructor) Implementation is {@code Impl}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2Ascii#getCurrentArgs()}
   */
  @Test
  public void testGetCurrentArgs_givenNative2AsciiImplementationIsImpl() {
    // Arrange
    Native2Ascii native2Ascii = new Native2Ascii();
    native2Ascii.setImplementation("Impl");

    // Act and Assert
    assertEquals(0, native2Ascii.getCurrentArgs().length);
  }

  /**
   * Test {@link Native2Ascii#createArg()}.
   * <p>
   * Method under test: {@link Native2Ascii#createArg()}
   */
  @Test
  public void testCreateArg() {
    // Arrange and Act
    ImplementationSpecificArgument actualCreateArgResult = (new Native2Ascii()).createArg();

    // Assert
    assertNull(actualCreateArgResult.getParts());
    Location location = actualCreateArgResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateArgResult.getDescription());
    assertNull(actualCreateArgResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link Native2Ascii#createImplementationClasspath()}.
   * <p>
   * Method under test: {@link Native2Ascii#createImplementationClasspath()}
   */
  @Test
  public void testCreateImplementationClasspath() {
    // Arrange and Act
    Path actualCreateImplementationClasspathResult = (new Native2Ascii()).createImplementationClasspath();

    // Assert
    Location location = actualCreateImplementationClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateImplementationClasspathResult.getDescription());
    assertNull(actualCreateImplementationClasspathResult.getProject());
    assertNull(actualCreateImplementationClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateImplementationClasspathResult.size());
    assertFalse(actualCreateImplementationClasspathResult.isReference());
    assertTrue(actualCreateImplementationClasspathResult.isEmpty());
  }
}
