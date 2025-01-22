package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Hashtable;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Sync.MyCopy;
import org.apache.tools.ant.taskdefs.Sync.SyncTarget;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.apache.tools.ant.util.ChainedMapper;
import org.apache.tools.ant.util.MergingMapper;
import org.junit.Test;

public class SyncDiffblueTest {
  /**
   * Test {@link Sync#addPreserveInTarget(SyncTarget)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Sync#addPreserveInTarget(SyncTarget)}
   */
  @Test
  public void testAddPreserveInTarget_thenThrowBuildException() {
    // Arrange
    Sync sync = new Sync();
    sync.addPreserveInTarget(new SyncTarget());

    // Act and Assert
    assertThrows(BuildException.class, () -> sync.addPreserveInTarget(new SyncTarget()));
  }

  /**
   * Test MyCopy getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link MyCopy#getIncludeEmptyDirs()}
   *   <li>{@link MyCopy#getToDir()}
   * </ul>
   */
  @Test
  public void testMyCopyGettersAndSetters() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    boolean actualIncludeEmptyDirs = myCopy.getIncludeEmptyDirs();

    // Assert
    assertNull(myCopy.getToDir());
    assertTrue(actualIncludeEmptyDirs);
  }

  /**
   * Test MyCopy new {@link MyCopy} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MyCopy}
   */
  @Test
  public void testMyCopyNewMyCopy() {
    // Arrange and Act
    MyCopy actualMyCopy = new MyCopy();

    // Assert
    assertNull(actualMyCopy.getToDir());
    assertNull(actualMyCopy.destFile);
    assertNull(actualMyCopy.file);
    assertNull(actualMyCopy.getDescription());
    assertNull(actualMyCopy.getTaskName());
    assertNull(actualMyCopy.getTaskType());
    assertNull(actualMyCopy.getEncoding());
    assertNull(actualMyCopy.getOutputEncoding());
    assertNull(actualMyCopy.getProject());
    assertNull(actualMyCopy.getOwningTarget());
    assertNull(actualMyCopy.mapperElement);
    assertEquals(3, actualMyCopy.verbosity);
    assertFalse(actualMyCopy.getForce());
    assertFalse(actualMyCopy.getPreserveLastModified());
    assertFalse(actualMyCopy.isEnableMultipleMapping());
    assertFalse(actualMyCopy.filtering);
    assertFalse(actualMyCopy.flatten);
    assertFalse(actualMyCopy.forceOverwrite);
    assertTrue(actualMyCopy.completeDirMap.isEmpty());
    assertTrue(actualMyCopy.dirCopyMap.isEmpty());
    assertTrue(actualMyCopy.fileCopyMap.isEmpty());
    assertTrue(actualMyCopy.getFilterChains().isEmpty());
    assertTrue(actualMyCopy.getFilterSets().isEmpty());
    assertTrue(actualMyCopy.filesets.isEmpty());
    assertTrue(actualMyCopy.rcs.isEmpty());
    assertTrue(actualMyCopy.getIncludeEmptyDirs());
    assertTrue(actualMyCopy.failonerror);
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    myCopy.setOverwrite(true);

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert
    Hashtable<String, String[]> stringStringArrayMap = myCopy.dirCopyMap;
    assertEquals(1, stringStringArrayMap.size());
    Hashtable<String, String[]> stringStringArrayMap2 = myCopy.fileCopyMap;
    assertEquals(1, stringStringArrayMap2.size());
    assertArrayEquals(new String[]{"/NULL_FILE/Dirs"}, stringStringArrayMap.get("/NULL_FILE/Dirs"));
    assertArrayEquals(new String[]{"/NULL_FILE/Files"}, stringStringArrayMap2.get("/NULL_FILE/Files"));
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs2() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER,
        new String[]{"."}, new String[]{"Dirs"});

    // Assert
    Hashtable<String, String[]> stringStringArrayMap = myCopy.fileCopyMap;
    assertEquals(1, stringStringArrayMap.size());
    assertArrayEquals(new String[]{"/NULL_FILE/."},
        stringStringArrayMap.get(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", ".").toString()));
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs3() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER,
        new String[]{".."}, new String[]{"Dirs"});

    // Assert
    Hashtable<String, String[]> stringStringArrayMap = myCopy.fileCopyMap;
    assertEquals(1, stringStringArrayMap.size());
    assertArrayEquals(new String[]{"/NULL_FILE/.."},
        stringStringArrayMap.get(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", "..").toString()));
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs4() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    MyCopy myCopy = new MyCopy();
    myCopy.setProject(project);

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs5() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), null, new String[]{"."},
        new String[]{"Dirs"});

    // Assert
    Hashtable<String, String[]> stringStringArrayMap = myCopy.fileCopyMap;
    assertEquals(1, stringStringArrayMap.size());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("user.dir"), ".").toString()},
        stringStringArrayMap.get(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", ".").toString()));
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link MyCopy} (default constructor) Flatten is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_givenMyCopyFlattenIsTrue() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    myCopy.setFlatten(true);

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link MyCopy} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_givenMyCopyProjectIsProject() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    myCopy.setProject(new Project());

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Then {@link MyCopy} (default constructor) {@link Copy#dirCopyMap} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_thenMyCopyDirCopyMapSizeIsOne() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    myCopy.setOverwrite(true);

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{null}, new String[]{"Dirs"});

    // Assert
    Hashtable<String, String[]> stringStringArrayMap = myCopy.dirCopyMap;
    assertEquals(1, stringStringArrayMap.size());
    assertArrayEquals(new String[]{"/NULL_FILE/Dirs"}, stringStringArrayMap.get("/NULL_FILE/Dirs"));
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_whenArrayOfStringWithDot() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"."}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_whenArrayOfStringWithDotDot() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{".."}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_whenArrayOfStringWithEmptyString() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{""}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>When array of {@link String} with empty string and {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_whenArrayOfStringWithEmptyStringAndDot() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"", "."}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_whenArrayOfStringWithFiles() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>When array of {@link String} with {@code No mapper} and {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_whenArrayOfStringWithNoMapperAndDot() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"No mapper", "."},
        new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>When array of {@link String} with {@code No mapper} and {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_whenArrayOfStringWithNoMapperAndDot2() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"No mapper", ".", "No mapper"},
        new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(File, File, String[], String[])}
   */
  @Test
  public void testMyCopyScanWithFromDirToDirFilesDirs_whenEmptyArrayOfString() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act
    myCopy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(myCopy.dirCopyMap.isEmpty());
    assertTrue(myCopy.fileCopyMap.isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new Resource("No mapper")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir2() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertEquals(1,
        myCopy
            .scan(new Resource[]{new Resource("No mapper", true, WaitFor.ONE_MILLISECOND)}, Copy.NULL_FILE_PLACEHOLDER)
            .size());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir3() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new FileResource(Copy.NULL_FILE_PLACEHOLDER)}, Copy.NULL_FILE_PLACEHOLDER)
        .isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir4() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new Resource("")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir5() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertEquals(1,
        myCopy.scan(new Resource[]{new Resource(".", true, WaitFor.ONE_MILLISECOND)}, Copy.NULL_FILE_PLACEHOLDER)
            .size());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir6() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertEquals(1,
        myCopy
            .scan(new Resource[]{new Resource("/NULL_FILE", true, WaitFor.ONE_MILLISECOND)}, Copy.NULL_FILE_PLACEHOLDER)
            .size());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir7() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertEquals(1,
        myCopy.scan(new Resource[]{new Resource("No mapper", true, Resource.UNKNOWN_SIZE)}, Copy.NULL_FILE_PLACEHOLDER)
            .size());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir8() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertEquals(1, myCopy.scan(new Resource[]{new Resource("No mapper", true, WaitFor.ONE_MILLISECOND)}, null).size());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir9() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertEquals(1,
        myCopy
            .scan(
                new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())},
                Copy.NULL_FILE_PLACEHOLDER)
            .size());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir10() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    JavaConstantResource javaConstantResource = new JavaConstantResource();

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{javaConstantResource, new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir11() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    Resource r = new Resource();

    // Act and Assert
    assertTrue(
        myCopy.scan(new Resource[]{new MappedResource(r, new ChainedMapper())}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link MyCopy} (default constructor) Flatten is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_givenMyCopyFlattenIsTrue() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    myCopy.setFlatten(true);

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link MyCopy} (default constructor) Overwrite is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_givenMyCopyOverwriteIsTrue() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    myCopy.setOverwrite(true);

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link MyCopy} (default constructor) Overwrite is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_givenMyCopyOverwriteIsTrue2() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    myCopy.setOverwrite(true);

    // Act and Assert
    assertEquals(1, myCopy.scan(new Resource[]{new Resource("No mapper")}, Copy.NULL_FILE_PLACEHOLDER).size());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link MyCopy} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_givenMyCopyProjectIsProject() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    myCopy.setProject(new Project());

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    MyCopy myCopy = new MyCopy();
    myCopy.setProject(project);

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link JavaConstantResource} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_whenArrayOfResourceWithJavaConstantResource() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new JavaConstantResource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource(String)} with name is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_whenArrayOfResourceWithResourceWithNameIsDot() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new Resource(".")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource(String)} with name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_whenArrayOfResourceWithResourceWithNameIsDotDot() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new Resource("..")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_whenArrayOfResourceWithResource_thenReturnEmpty() {
    // Arrange
    MyCopy myCopy = new MyCopy();

    // Act and Assert
    assertTrue(myCopy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>When empty array of {@link Resource}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_whenEmptyArrayOfResource_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new MyCopy()).scan(new Resource[]{}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test MyCopy {@link MyCopy#scan(Resource[], File)} with {@code resources}, {@code toDir}.
   * <ul>
   *   <li>When {@link MergingMapper#MergingMapper(String)} with to is {@code alice.liddell@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MyCopy#scan(Resource[], File)}
   */
  @Test
  public void testMyCopyScanWithResourcesToDir_whenMergingMapperWithToIsAliceLiddellExampleOrg() {
    // Arrange
    MyCopy myCopy = new MyCopy();
    Resource r = new Resource();

    // Act and Assert
    assertEquals(1,
        myCopy
            .scan(new Resource[]{new MappedResource(r, new MergingMapper("alice.liddell@example.org"))},
                Copy.NULL_FILE_PLACEHOLDER)
            .size());
  }

  /**
   * Test MyCopy {@link MyCopy#supportsNonFileResources()}.
   * <p>
   * Method under test: {@link MyCopy#supportsNonFileResources()}
   */
  @Test
  public void testMyCopySupportsNonFileResources() {
    // Arrange, Act and Assert
    assertTrue((new MyCopy()).supportsNonFileResources());
  }

  /**
   * Test new {@link Sync} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Sync}
   */
  @Test
  public void testNewSync() {
    // Arrange and Act
    Sync actualSync = new Sync();

    // Assert
    Location location = actualSync.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSync.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSync.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSync.getTaskName());
    assertNull(actualSync.getTaskType());
    assertNull(actualSync.getProject());
    assertNull(actualSync.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSync, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test SyncTarget {@link SyncTarget#getPreserveEmptyDirs()}.
   * <p>
   * Method under test: {@link SyncTarget#getPreserveEmptyDirs()}
   */
  @Test
  public void testSyncTargetGetPreserveEmptyDirs() {
    // Arrange, Act and Assert
    assertNull((new SyncTarget()).getPreserveEmptyDirs());
  }

  /**
   * Test SyncTarget new {@link SyncTarget} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SyncTarget}
   */
  @Test
  public void testSyncTargetNewSyncTarget() {
    // Arrange and Act
    SyncTarget actualSyncTarget = new SyncTarget();

    // Assert
    assertNull(actualSyncTarget.getDir());
    assertNull(actualSyncTarget.getPreserveEmptyDirs());
    Location location = actualSyncTarget.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSyncTarget.getDescription());
    assertNull(actualSyncTarget.getProject());
    assertNull(actualSyncTarget.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(5, actualSyncTarget.getMaxLevelsOfSymlinks());
    assertFalse(actualSyncTarget.isReference());
    assertTrue(actualSyncTarget.getDefaultexcludes());
    assertTrue(actualSyncTarget.getErrorOnMissingDir());
  }

  /**
   * Test SyncTarget {@link SyncTarget#setDir(File)}.
   * <p>
   * Method under test: {@link SyncTarget#setDir(File)}
   */
  @Test
  public void testSyncTargetSetDir() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SyncTarget()).setDir(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test SyncTarget {@link SyncTarget#setPreserveEmptyDirs(boolean)}.
   * <p>
   * Method under test: {@link SyncTarget#setPreserveEmptyDirs(boolean)}
   */
  @Test
  public void testSyncTargetSetPreserveEmptyDirs() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.setPreserveEmptyDirs(true);

    // Assert
    assertTrue(syncTarget.getPreserveEmptyDirs());
  }
}
