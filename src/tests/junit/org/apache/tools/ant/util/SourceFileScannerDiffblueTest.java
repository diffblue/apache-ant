package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.filters.ClassConstants;
import org.apache.tools.ant.filters.ExpandProperties;
import org.apache.tools.ant.filters.HeadFilter;
import org.apache.tools.ant.filters.TokenFilter;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.optional.ScriptMapper;
import org.junit.Test;

public class SourceFileScannerDiffblueTest {
  /**
   * Test {@link SourceFileScanner#SourceFileScanner(Task)}.
   * <p>
   * Method under test: {@link SourceFileScanner#SourceFileScanner(Task)}
   */
  @Test
  public void testNewSourceFileScanner() {
    // Arrange and Act
    SourceFileScanner actualSourceFileScanner = new SourceFileScanner(new TaskAdapter());

    // Assert
    Task task = actualSourceFileScanner.task;
    assertTrue(task instanceof TaskAdapter);
    RuntimeConfigurable runtimeConfigurableWrapper = task.getRuntimeConfigurableWrapper();
    assertEquals("", runtimeConfigurableWrapper.getText().toString());
    assertNull(((TaskAdapter) task).getProxy());
    Location location = task.getLocation();
    assertNull(location.getFileName());
    assertNull(task.getDescription());
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(task.getTaskName());
    assertNull(task.getTaskType());
    assertNull(task.getProject());
    assertNull(task.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    Task expectedProxy = actualSourceFileScanner.task;
    assertSame(expectedProxy, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{""}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".", "var"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity3() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".", "var", "."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity4() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());
    SourceFileScanner sourceFileScanner = new SourceFileScanner(task);
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity5() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "Warning: ").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity6() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{""}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity7() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"..", "var"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity8() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".", ""}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_givenClassConstants() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_givenClassConstants2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_givenExpandProperties() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_givenExpandProperties2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_givenHeadFilter() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_givenHeadFilter2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addHeadFilter(new HeadFilter());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_givenProject() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.setProject(new Project());
    mapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_givenTokenFilter() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addTokenFilter(new TokenFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_givenTokenFilter2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addTokenFilter(new TokenFilter());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_whenArrayOfStringWithDot() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_whenArrayOfStringWithDot2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"."}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_whenArrayOfStringWithDotDot() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When {@link ChainedMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_whenChainedMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new ChainedMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_whenCutDirsMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_whenEmptyArrayOfString() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When {@link FilterMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_whenFilterMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When {@link ScriptMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapperGranularity_whenScriptMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new ScriptMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenClassConstants() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenClassConstants2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenExpandProperties() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenExpandProperties2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenHeadFilter() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenHeadFilter2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addHeadFilter(new HeadFilter());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenProject() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.setProject(new Project());
    mapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenTaskAdapterProjectIsProject() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());
    SourceFileScanner sourceFileScanner = new SourceFileScanner(task);
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenTokenFilter() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addTokenFilter(new TokenFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_givenTokenFilter2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addTokenFilter(new TokenFilter());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDot() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDot2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"."}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code .} and empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDotAndEmptyString() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".", ""}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code .} and {@code var}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDotAndVar() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".", "var"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code .} and {@code var}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDotAndVar2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrict(new String[]{".", "var", "."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDotDot() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{".."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenArrayOfStringWithEmptyString() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{""}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenArrayOfStringWithEmptyString2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{""}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link ChainedMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenChainedMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new ChainedMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenCutDirsMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenEmptyArrayOfString() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link FilterMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenFilterMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link ScriptMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrict(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictWithFilesSrcDirDestDirMapper_whenScriptMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrict(new String[]{"Files"}, srcDir, destDir, new ScriptMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{".", ""}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity3() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{""}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity4() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity5() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".", "var"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity6() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".", "var", "."}, srcDir, destDir,
        new CutDirsMapper(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity7() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());
    SourceFileScanner sourceFileScanner = new SourceFileScanner(task);
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity8() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"."}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity9() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "Warning: ").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".."}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity10() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{""}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity11() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".", ""}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_givenClassConstants() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_givenClassConstants2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_givenExpandProperties() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_givenExpandProperties2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_givenHeadFilter() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_givenHeadFilter2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addHeadFilter(new HeadFilter());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_givenProject() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.setProject(new Project());
    mapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_givenTokenFilter() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addTokenFilter(new TokenFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_givenTokenFilter2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addTokenFilter(new TokenFilter());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When {@link ChainedMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_whenChainedMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new ChainedMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_whenCutDirsMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When {@link FilterMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_whenFilterMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new FilterMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When {@link Long#MAX_VALUE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_whenMax_value() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".", "var"}, srcDir, destDir, new CutDirsMapper(),
        Long.MAX_VALUE).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}, {@code granularity}.
   * <ul>
   *   <li>When {@link ScriptMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper, long)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapperGranularity_whenScriptMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new ScriptMapper(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenClassConstants() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenClassConstants2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenExpandProperties() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenExpandProperties2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenHeadFilter() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenHeadFilter2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addHeadFilter(new HeadFilter());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenProject() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.setProject(new Project());
    mapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenTaskAdapterProjectIsProject() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());
    SourceFileScanner sourceFileScanner = new SourceFileScanner(task);
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenTokenFilter() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addTokenFilter(new TokenFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_givenTokenFilter2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addTokenFilter(new TokenFilter());
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, mapper).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDot() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDot2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{"."}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code .} and {@code var}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDotAndVar() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{".", "var"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code .} and {@code var}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDotAndVar2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{".", "var", "."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenArrayOfStringWithDotDot() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{".."}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenArrayOfStringWithEmptyString() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{""}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenArrayOfStringWithEmptyString2() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{""}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link ChainedMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenChainedMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new ChainedMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenCutDirsMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenEmptyArrayOfString() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, sourceFileScanner.restrictAsFiles(new String[]{}, srcDir, destDir, new CutDirsMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link FilterMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenFilterMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new FilterMapper()).length);
  }

  /**
   * Test {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)} with {@code files}, {@code srcDir}, {@code destDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link ScriptMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SourceFileScanner#restrictAsFiles(String[], File, File, FileNameMapper)}
   */
  @Test
  public void testRestrictAsFilesWithFilesSrcDirDestDirMapper_whenScriptMapper() {
    // Arrange
    SourceFileScanner sourceFileScanner = new SourceFileScanner(new TaskAdapter());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        sourceFileScanner.restrictAsFiles(new String[]{"Files"}, srcDir, destDir, new ScriptMapper()).length);
  }
}
