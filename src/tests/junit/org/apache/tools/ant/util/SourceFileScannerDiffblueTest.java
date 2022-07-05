package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.ant.antunit.AntUnit;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.filters.ClassConstants;
import org.apache.tools.ant.filters.ExpandProperties;
import org.apache.tools.ant.filters.HeadFilter;
import org.apache.tools.ant.filters.TokenFilter;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.optional.ScriptMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class SourceFileScannerDiffblueTest {
  /**
  * Method under test: {@link SourceFileScanner#SourceFileScanner(Task)}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    Task task = (new SourceFileScanner(new AntUnit())).task;
    assertTrue(task instanceof AntUnit);
    assertNull(task.getDescription());
    assertNull(task.getTaskType());
    assertNull(task.getTaskName());
    assertNull(task.getProject());
    assertNull(task.getOwningTarget());
    Location location = task.getLocation();
    assertEquals(0, location.getLineNumber());
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
    RuntimeConfigurable runtimeConfigurableWrapper = task.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(task, runtimeConfigurableWrapper.getProxy());
    StringBuffer text = runtimeConfigurableWrapper.getText();
    assertEquals(0, text.capacity());
    assertEquals(0, text.length());
  }

  /**
   * Method under test: {@link SourceFileScanner#getResource(String)}
   */
  @Test
  public void testGetResource() {
    // Arrange and Act
    Resource actualResource = (new SourceFileScanner(new AntUnit())).getResource("Name");

    // Assert
    assertNull(((FileResource) actualResource).getBaseDir());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("user.dir"), "Name").toString(), "\"");
    assertEquals(expectedToLongStringResult, actualResource.toLongString());
    assertTrue(actualResource.isFilesystemOnly());
  }

  /**
   * Method under test: {@link SourceFileScanner#getResource(String)}
   */
  @Test
  public void testGetResource2() {
    // Arrange and Act
    Resource actualResource = (new SourceFileScanner(new AntUnit())).getResource(".");

    // Assert
    assertNull(((FileResource) actualResource).getBaseDir());
    String expectedToLongStringResult = String.join("", "FileResource \"", System.getProperty("user.dir"), "\"");
    assertEquals(expectedToLongStringResult, actualResource.toLongString());
    assertTrue(actualResource.isFilesystemOnly());
  }

  /**
   * Method under test: {@link SourceFileScanner#getResource(String)}
   */
  @Test
  public void testGetResource3() {
    // Arrange and Act
    Resource actualResource = (new SourceFileScanner(new AntUnit())).getResource("..");

    // Assert
    assertNull(((FileResource) actualResource).getBaseDir());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("user.home"), "projects").toString(), "\"");
    assertEquals(expectedToLongStringResult, actualResource.toLongString());
    assertTrue(actualResource.isFilesystemOnly());
  }

  /**
   * Method under test: {@link SourceFileScanner#getResource(String)}
   */
  @Test
  public void testGetResource4() {
    // Arrange and Act
    Resource actualResource = (new SourceFileScanner(new AntUnit())).getResource("");

    // Assert
    assertNull(((FileResource) actualResource).getBaseDir());
    String expectedToLongStringResult = String.join("", "FileResource \"", System.getProperty("user.dir"), "\"");
    assertEquals(expectedToLongStringResult, actualResource.toLongString());
    assertTrue(actualResource.isFilesystemOnly());
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict3() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict4() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{""}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict5() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict6() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".", "."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict7() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict8() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new ScriptMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict9() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new ChainedMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict10() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());
    SourceFileScanner sourceFileScanner = new SourceFileScanner(antUnit);
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict11() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".."}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict12() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{""}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict13() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"", "."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict14() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addClassConstants(new ClassConstants());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict15() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict16() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict17() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addTokenFilter(new TokenFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict18() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.setProject(new Project());
    filterMapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrict19() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addClassConstants(new ClassConstants());
    filterMapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict20() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict21() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict22() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict23() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{""}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict24() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict25() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".", "."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict26() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict27() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new ScriptMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict28() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new ChainedMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict29() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());
    SourceFileScanner sourceFileScanner = new SourceFileScanner(antUnit);
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict30() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get("Warning: ", "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict31() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".."}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict32() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{""}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict33() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"", "."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict34() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addClassConstants(new ClassConstants());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict35() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict36() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrict37() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addTokenFilter(new TokenFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, filterMapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles3() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles4() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{""}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles5() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles6() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{".", "."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles7() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles8() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new ScriptMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles9() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new ChainedMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles10() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());
    SourceFileScanner sourceFileScanner = new SourceFileScanner(antUnit);
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles11() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".."}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles12() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{""}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles13() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"", "."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles14() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addClassConstants(new ClassConstants());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles15() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles16() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles17() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addTokenFilter(new TokenFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFiles18() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.setProject(new Project());
    filterMapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, filterMapper).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles19() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles20() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles21() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles22() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{""}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles23() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles24() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".", "."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles25() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles26() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new ScriptMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles27() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new ChainedMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles28() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());
    SourceFileScanner sourceFileScanner = new SourceFileScanner(antUnit);
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles29() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get("Warning: ", "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles30() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".."}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles31() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{""}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles32() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"..", "."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles33() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"", "."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles34() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addClassConstants(new ClassConstants());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, filterMapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles35() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, filterMapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles36() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, filterMapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFiles37() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new AntUnit());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addTokenFilter(new TokenFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, filterMapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }
}

