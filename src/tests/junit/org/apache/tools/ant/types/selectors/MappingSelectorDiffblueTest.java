package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.filters.ExpandProperties;
import org.apache.tools.ant.types.AntFilterReader;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.IdentityMapper;
import org.junit.Test;

public class MappingSelectorDiffblueTest {
  /**
   * Test {@link MappingSelector#setTargetdir(File)}.
   * <p>
   * Method under test: {@link MappingSelector#setTargetdir(File)}
   */
  @Test
  public void testSetTargetdir() {
    // Arrange
    DependSelector dependSelector = new DependSelector();

    // Act
    dependSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    File file = dependSelector.targetdir;
    assertEquals("test.txt", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link MappingSelector#createMapper()}.
   * <ul>
   *   <li>Then {@link DependSelector} (default constructor) {@link MappingSelector#mapperElement} Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#createMapper()}
   */
  @Test
  public void testCreateMapper_thenDependSelectorMapperElementDescriptionIsNull() throws BuildException {
    // Arrange
    DependSelector dependSelector = new DependSelector();

    // Act
    Mapper actualCreateMapperResult = dependSelector.createMapper();

    // Assert
    Mapper mapper = dependSelector.mapperElement;
    assertNull(mapper.getDescription());
    assertNull(mapper.getProject());
    assertNull(mapper.getRefid());
    assertFalse(mapper.isReference());
    assertSame(dependSelector.mapperElement, actualCreateMapperResult);
  }

  /**
   * Test {@link MappingSelector#createMapper()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#createMapper()}
   */
  @Test
  public void testCreateMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    dependSelector.addConfigured(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> dependSelector.createMapper());
  }

  /**
   * Test {@link MappingSelector#addConfigured(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link DependSelector} (default constructor).</li>
   *   <li>Then {@link DependSelector} (default constructor) {@link MappingSelector#map} {@link CutDirsMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured_givenDependSelector_thenDependSelectorMapCutDirsMapper() {
    // Arrange
    DependSelector dependSelector = new DependSelector();

    // Act
    dependSelector.addConfigured(new CutDirsMapper());

    // Assert
    assertTrue(dependSelector.map instanceof CutDirsMapper);
  }

  /**
   * Test {@link MappingSelector#addConfigured(FileNameMapper)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured_thenThrowBuildException() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    dependSelector.addConfigured(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> dependSelector.addConfigured(new CutDirsMapper()));
  }

  /**
   * Test {@link MappingSelector#verifySettings()}.
   * <p>
   * Method under test: {@link MappingSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    dependSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    dependSelector.addConfigured(new CutDirsMapper());

    // Act
    dependSelector.verifySettings();

    // Assert that nothing has changed
    assertTrue(dependSelector.map instanceof CutDirsMapper);
  }

  /**
   * Test {@link MappingSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link DependSelector} (default constructor) Error is {@code The targetdir attribute is required.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenDependSelectorErrorIsTheTargetdirAttributeIsRequired() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    dependSelector.setError("The targetdir attribute is required.");
    dependSelector.addConfigured(new CutDirsMapper());

    // Act
    dependSelector.verifySettings();

    // Assert that nothing has changed
    assertTrue(dependSelector.map instanceof CutDirsMapper);
    assertEquals("The targetdir attribute is required.", dependSelector.getError());
  }

  /**
   * Test {@link MappingSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link DependSelector} (default constructor).</li>
   *   <li>Then {@link DependSelector} (default constructor) {@link MappingSelector#map} {@link IdentityMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenDependSelector_thenDependSelectorMapIdentityMapper() {
    // Arrange
    DependSelector dependSelector = new DependSelector();

    // Act
    dependSelector.verifySettings();

    // Assert
    assertTrue(dependSelector.map instanceof IdentityMapper);
    assertEquals("The targetdir attribute is required.", dependSelector.getError());
  }

  /**
   * Test {@link MappingSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link DependSelector} (default constructor) {@link MappingSelector#map} {@link CutDirsMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenDependSelectorMapCutDirsMapper() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    dependSelector.addConfigured(new CutDirsMapper());

    // Act
    dependSelector.verifySettings();

    // Assert
    assertTrue(dependSelector.map instanceof CutDirsMapper);
    assertEquals("The targetdir attribute is required.", dependSelector.getError());
  }

  /**
   * Test {@link MappingSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link MappingSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addFilterReader(new AntFilterReader());

    DependSelector dependSelector = new DependSelector();
    dependSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    dependSelector.addConfigured(fileNameMapper);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(dependSelector.map instanceof FilterMapper);
    assertTrue(dependSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MappingSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link MappingSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile2() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addExpandProperties(new ExpandProperties());

    DependSelector dependSelector = new DependSelector();
    dependSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    dependSelector.addConfigured(fileNameMapper);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(dependSelector.map instanceof FilterMapper);
    assertTrue(dependSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MappingSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenFilterMapperProjectIsProject() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.setProject(new Project());
    fileNameMapper.addExpandProperties(new ExpandProperties());

    DependSelector dependSelector = new DependSelector();
    dependSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    dependSelector.addConfigured(fileNameMapper);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(dependSelector.map instanceof FilterMapper);
    assertTrue(dependSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MappingSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then {@link DependSelector} (default constructor) {@link MappingSelector#map} {@link CompositeMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenDependSelectorMapCompositeMapper() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    dependSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    dependSelector.addConfigured(new CompositeMapper());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(dependSelector.map instanceof CompositeMapper);
    assertFalse(dependSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MappingSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then {@link DependSelector} (default constructor) {@link MappingSelector#map} {@link FilterMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenDependSelectorMapFilterMapper() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    dependSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    dependSelector.addConfigured(new FilterMapper());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(dependSelector.map instanceof FilterMapper);
    assertTrue(dependSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MappingSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappingSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenEmptyString_thenReturnFalse() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    dependSelector.setTargetdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    dependSelector.addConfigured(new FilterMapper());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(dependSelector.map instanceof FilterMapper);
    assertFalse(
        dependSelector.isSelected(basedir, "", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MappingSelector#setGranularity(int)}.
   * <p>
   * Method under test: {@link MappingSelector#setGranularity(int)}
   */
  @Test
  public void testSetGranularity() {
    // Arrange
    DependSelector dependSelector = new DependSelector();

    // Act
    dependSelector.setGranularity(1);

    // Assert
    assertEquals(1, dependSelector.granularity);
  }
}
