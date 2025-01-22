package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.filters.ExpandProperties;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.apache.tools.ant.types.AntFilterReader;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.selectors.PresentSelector.FilePresence;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class PresentSelectorDiffblueTest {
  /**
   * Test FilePresence {@link FilePresence#getValues()}.
   * <p>
   * Method under test: {@link FilePresence#getValues()}
   */
  @Test
  public void testFilePresenceGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"srconly", "both"}, (new FilePresence()).getValues());
  }

  /**
   * Test FilePresence new {@link FilePresence} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FilePresence}
   */
  @Test
  public void testFilePresenceNewFilePresence() {
    // Arrange and Act
    FilePresence actualFilePresence = new FilePresence();

    // Assert
    assertNull(actualFilePresence.getValue());
    assertEquals(-1, actualFilePresence.getIndex());
  }

  /**
   * Test {@link PresentSelector#toString()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#toString()}
   */
  @Test
  public void testToString_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("NOT YET SET", typeClass);
    project.addBuildListener(new AntClassLoader());

    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.setProject(project);

    PresentSelector presentSelector = new PresentSelector();
    presentSelector.addConfigured(fileNameMapper);

    // Act and Assert
    assertEquals("{presentselector targetdir: NOT YET SET present: bothFilterMapper}", presentSelector.toString());
  }

  /**
   * Test {@link PresentSelector#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#toString()}
   */
  @Test
  public void testToString_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.setProject(project);

    PresentSelector presentSelector = new PresentSelector();
    presentSelector.addConfigured(fileNameMapper);

    // Act and Assert
    assertEquals("{presentselector targetdir: NOT YET SET present: bothFilterMapper}", presentSelector.toString());
  }

  /**
   * Test {@link PresentSelector#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link BigProjectLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#toString()}
   */
  @Test
  public void testToString_givenProjectAddBuildListenerBigProjectLogger() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new BigProjectLogger());

    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.setProject(project);

    PresentSelector presentSelector = new PresentSelector();
    presentSelector.addConfigured(fileNameMapper);

    // Act and Assert
    assertEquals("{presentselector targetdir: NOT YET SET present: bothFilterMapper}", presentSelector.toString());
  }

  /**
   * Test {@link PresentSelector#toString()}.
   * <ul>
   *   <li>Then return a string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#toString()}
   */
  @Test
  public void testToString_thenReturnAString() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.setDescription("The characteristics of someone or something");

    PresentSelector presentSelector = new PresentSelector();
    presentSelector.addConfigured(fileNameMapper);

    // Act and Assert
    assertEquals("{presentselector targetdir: NOT YET SET present: bothFilterMapper The characteristics of someone or"
        + " something}", presentSelector.toString());
  }

  /**
   * Test {@link PresentSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {presentselector targetdir: NOT YET SET present: both}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#toString()}
   */
  @Test
  public void testToString_thenReturnPresentselectorTargetdirNotYetSetPresentBoth() {
    // Arrange, Act and Assert
    assertEquals("{presentselector targetdir: NOT YET SET present: both}", (new PresentSelector()).toString());
  }

  /**
   * Test {@link PresentSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {presentselector targetdir: NOT YET SET present: bothFilterMapper}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#toString()}
   */
  @Test
  public void testToString_thenReturnPresentselectorTargetdirNotYetSetPresentBothFilterMapper() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.addConfigured(new FilterMapper());

    // Act and Assert
    assertEquals("{presentselector targetdir: NOT YET SET present: bothFilterMapper}", presentSelector.toString());
  }

  /**
   * Test {@link PresentSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {presentselector targetdir: NOT YET SET present: bothFilterMapper}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#toString()}
   */
  @Test
  public void testToString_thenReturnPresentselectorTargetdirNotYetSetPresentBothFilterMapper2() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.setProject(new Project());

    PresentSelector presentSelector = new PresentSelector();
    presentSelector.addConfigured(fileNameMapper);

    // Act and Assert
    assertEquals("{presentselector targetdir: NOT YET SET present: bothFilterMapper}", presentSelector.toString());
  }

  /**
   * Test {@link PresentSelector#createMapper()}.
   * <ul>
   *   <li>Given {@link PresentSelector} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#createMapper()}
   */
  @Test
  public void testCreateMapper_givenPresentSelector_thenReturnLocationFileNameIsNull() throws BuildException {
    // Arrange and Act
    Mapper actualCreateMapperResult = (new PresentSelector()).createMapper();

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
   * Test {@link PresentSelector#createMapper()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#createMapper()}
   */
  @Test
  public void testCreateMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.addConfigured(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> presentSelector.createMapper());
  }

  /**
   * Test {@link PresentSelector#addConfigured(FileNameMapper)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured_thenThrowBuildException() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.addConfigured(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> presentSelector.addConfigured(new CutDirsMapper()));
  }

  /**
   * Test {@link PresentSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link PresentSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenPresentSelector() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();

    // Act
    presentSelector.verifySettings();

    // Assert
    assertEquals("The targetdir attribute is required.", presentSelector.getError());
  }

  /**
   * Test {@link PresentSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link PresentSelector} (default constructor) Error is {@code The targetdir attribute is required.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenPresentSelectorErrorIsTheTargetdirAttributeIsRequired() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setError("The targetdir attribute is required.");
    presentSelector.addConfigured(new CutDirsMapper());

    // Act
    presentSelector.verifySettings();

    // Assert that nothing has changed
    assertEquals("The targetdir attribute is required.", presentSelector.getError());
  }

  /**
   * Test {@link PresentSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link PresentSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenPresentSelectorErrorIsNull() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    presentSelector.addConfigured(new CutDirsMapper());

    // Act
    presentSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(presentSelector.getError());
  }

  /**
   * Test {@link PresentSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link PresentSelector} (default constructor) Error is {@code The targetdir attribute is required.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenPresentSelectorErrorIsTheTargetdirAttributeIsRequired() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.addConfigured(new CutDirsMapper());

    // Act
    presentSelector.verifySettings();

    // Assert
    assertEquals("The targetdir attribute is required.", presentSelector.getError());
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    presentSelector.addConfigured(new CompositeMapper());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(presentSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile2() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(presentSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile3() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(presentSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile4() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addFilterReader(new AntFilterReader());

    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    presentSelector.addConfigured(fileNameMapper);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(presentSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile5() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addExpandProperties(new ExpandProperties());

    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    presentSelector.addConfigured(fileNameMapper);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(presentSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenFilterMapperProjectIsProject() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.setProject(new Project());
    fileNameMapper.addExpandProperties(new ExpandProperties());

    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    presentSelector.addConfigured(fileNameMapper);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(presentSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenReturnFalse() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(presentSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenReturnFalse2() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    presentSelector.addConfigured(new FilterMapper());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(presentSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenDot_thenReturnTrue() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        presentSelector.isSelected(basedir, ".", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenEmptyString_thenReturnFalse() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    presentSelector.addConfigured(new FilterMapper());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        presentSelector.isSelected(basedir, "", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PresentSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PresentSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenEmptyString_thenReturnTrue() {
    // Arrange
    PresentSelector presentSelector = new PresentSelector();
    presentSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        presentSelector.isSelected(basedir, "", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test new {@link PresentSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link PresentSelector}
   */
  @Test
  public void testNewPresentSelector() {
    // Arrange and Act
    PresentSelector actualPresentSelector = new PresentSelector();

    // Assert
    Location location = actualPresentSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualPresentSelector.getDescription());
    assertNull(actualPresentSelector.getError());
    assertNull(actualPresentSelector.getProject());
    assertNull(actualPresentSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualPresentSelector.isReference());
  }
}
