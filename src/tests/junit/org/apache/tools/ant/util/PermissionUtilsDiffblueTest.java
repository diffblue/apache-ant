package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import org.apache.ant.antunit.LogContent;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.optional.xz.XzResource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Ignore;
import org.junit.Test;

public class PermissionUtilsDiffblueTest {
  /**
   * Method under test: {@link PermissionUtils.FileType#of(Path)}
   */
  @Test
  public void testFileTypeOf() throws IOException {
    // Arrange, Act and Assert
    assertEquals(PermissionUtils.FileType.DIR,
        PermissionUtils.FileType.of(Paths.get(System.getProperty("java.io.tmpdir"), "")));
    assertEquals(PermissionUtils.FileType.REGULAR_FILE, PermissionUtils.FileType.of(new Resource("Name")));
    assertEquals(PermissionUtils.FileType.REGULAR_FILE, PermissionUtils.FileType.of(new Resource()));
    assertEquals(PermissionUtils.FileType.DIR,
        PermissionUtils.FileType.of(new Resource("Name", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, true)));
    assertEquals(PermissionUtils.FileType.REGULAR_FILE, PermissionUtils.FileType.of(new XzResource(new LogContent())));
  }

  /**
   * Method under test: {@link PermissionUtils.FileType#of(Resource)}
   */
  @Test
  public void testFileTypeOf2() {
    // Arrange
    LogContent logContent = new LogContent();
    logContent.setDirectory(true);

    // Act and Assert
    assertEquals(PermissionUtils.FileType.DIR, PermissionUtils.FileType.of(new XzResource(logContent)));
  }

  /**
   * Method under test: {@link PermissionUtils.FileType#of(Resource)}
   */
  @Test
  public void testFileTypeOf3() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertEquals(PermissionUtils.FileType.REGULAR_FILE, PermissionUtils.FileType.of(fileResource));
    assertEquals(0L, fileResource.getLastModified());
    assertEquals("file attribute is null!", fileResource.getName());
  }

  /**
  * Method under test: {@link PermissionUtils#getPermissions(Resource, Function)}
  */
  @Test
  public void testGetPermissions() throws IOException {
    // Arrange, Act and Assert
    assertTrue(PermissionUtils.getPermissions(new Resource("Name"), null).isEmpty());
    assertTrue(PermissionUtils.getPermissions(new XzResource(), null).isEmpty());
  }

  /**
   * Method under test: {@link PermissionUtils#getPermissions(Resource, Function)}
   */
  @Test
  @Ignore
  public void testGetPermissions2() throws IOException {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissions = PermissionUtils
        .getPermissions(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "").toFile()), null);

    // Assert
    assertEquals(3, actualPermissions.size());
    assertTrue(actualPermissions.contains(PosixFilePermission.OWNER_WRITE));
    assertTrue(actualPermissions.contains(PosixFilePermission.OWNER_EXECUTE));
    assertTrue(actualPermissions.contains(PosixFilePermission.OWNER_READ));
  }

  /**
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, PermissionUtils.FileType)}
   */
  @Test
  public void testModeFromPermissions() {
    // Arrange, Act and Assert
    assertEquals(32768, PermissionUtils.modeFromPermissions(new HashSet<>(), PermissionUtils.FileType.REGULAR_FILE));
    assertEquals(16384, PermissionUtils.modeFromPermissions(new HashSet<>(), PermissionUtils.FileType.DIR));
    assertEquals(40960, PermissionUtils.modeFromPermissions(new HashSet<>(), PermissionUtils.FileType.SYMLINK));
    assertEquals(0, PermissionUtils.modeFromPermissions(new HashSet<>(), PermissionUtils.FileType.OTHER));
  }

  /**
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, PermissionUtils.FileType)}
   */
  @Test
  public void testModeFromPermissions2() {
    // Arrange
    HashSet<PosixFilePermission> posixFilePermissionSet = new HashSet<>();
    posixFilePermissionSet.add(PosixFilePermission.OWNER_READ);

    // Act and Assert
    assertEquals(33024,
        PermissionUtils.modeFromPermissions(posixFilePermissionSet, PermissionUtils.FileType.REGULAR_FILE));
  }

  /**
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, PermissionUtils.FileType)}
   */
  @Test
  public void testModeFromPermissions3() {
    // Arrange
    HashSet<PosixFilePermission> posixFilePermissionSet = new HashSet<>();
    posixFilePermissionSet.add(PosixFilePermission.OWNER_WRITE);

    // Act and Assert
    assertEquals(32896,
        PermissionUtils.modeFromPermissions(posixFilePermissionSet, PermissionUtils.FileType.REGULAR_FILE));
  }

  /**
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, PermissionUtils.FileType)}
   */
  @Test
  public void testModeFromPermissions4() {
    // Arrange
    HashSet<PosixFilePermission> posixFilePermissionSet = new HashSet<>();
    posixFilePermissionSet.add(PosixFilePermission.OWNER_EXECUTE);

    // Act and Assert
    assertEquals(32832,
        PermissionUtils.modeFromPermissions(posixFilePermissionSet, PermissionUtils.FileType.REGULAR_FILE));
  }

  /**
   * Method under test: {@link PermissionUtils#permissionsFromMode(int)}
   */
  @Test
  public void testPermissionsFromMode() {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissionsFromModeResult = PermissionUtils.permissionsFromMode(1);

    // Assert
    assertEquals(1, actualPermissionsFromModeResult.size());
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_EXECUTE));
  }

  /**
   * Method under test: {@link PermissionUtils#permissionsFromMode(int)}
   */
  @Test
  public void testPermissionsFromMode2() {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissionsFromModeResult = PermissionUtils.permissionsFromMode(2);

    // Assert
    assertEquals(1, actualPermissionsFromModeResult.size());
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_WRITE));
  }

  /**
   * Method under test: {@link PermissionUtils#permissionsFromMode(int)}
   */
  @Test
  public void testPermissionsFromMode3() {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissionsFromModeResult = PermissionUtils
        .permissionsFromMode(Retryable.RETRY_FOREVER);

    // Assert
    assertEquals(9, actualPermissionsFromModeResult.size());
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OWNER_READ));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OWNER_WRITE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OWNER_EXECUTE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.GROUP_READ));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.GROUP_WRITE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.GROUP_EXECUTE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_READ));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_WRITE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_EXECUTE));
  }

  /**
   * Method under test: {@link PermissionUtils#setPermissions(Resource, Set, Consumer)}
   */
  @Test
  public void testSetPermissions() throws IOException {
    // Arrange
    Resource resource = new Resource("Name");

    // Act
    PermissionUtils.setPermissions(resource, new HashSet<>(), null);

    // Assert that nothing has changed
    assertEquals(1, resource.size());
    assertEquals(0L, resource.getSize());
    assertEquals("Name", resource.getName());
  }

  /**
   * Method under test: {@link PermissionUtils#setPermissions(Resource, Set, Consumer)}
   */
  @Test
  public void testSetPermissions2() throws IOException {
    // Arrange
    XzResource xzResource = new XzResource();

    // Act
    PermissionUtils.setPermissions(xzResource, new HashSet<>(), null);

    // Assert that nothing has changed
    assertEquals(1, xzResource.size());
  }
}

