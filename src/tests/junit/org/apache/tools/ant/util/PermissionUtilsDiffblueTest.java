package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.BZip2Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.apache.tools.ant.util.PermissionUtils.FileType;
import org.junit.Test;

public class PermissionUtilsDiffblueTest {
  /**
   * Test FileType {@link FileType#of(Path)} with {@code p}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt}.</li>
   *   <li>Then return {@code DIR}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileType#of(Path)}
   */
  @Test
  public void testFileTypeOfWithP_whenPropertyIsJavaIoTmpdirIsTestTxt_thenReturnDir() throws IOException {
    // Arrange, Act and Assert
    assertEquals(FileType.DIR, FileType.of(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt")));
  }

  /**
   * Test FileType {@link FileType#of(Resource)} with {@code r}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileType#of(Resource)}
   */
  @Test
  public void testFileTypeOfWithR_givenFileAttributeIsNull() {
    // Arrange
    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertEquals(FileType.REGULAR_FILE, FileType.of(r));
  }

  /**
   * Test FileType {@link FileType#of(Resource)} with {@code r}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   *   <li>Then return {@code DIR}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileType#of(Resource)}
   */
  @Test
  public void testFileTypeOfWithR_givenTrue_whenResourceDirectoryIsTrue_thenReturnDir() {
    // Arrange
    Resource r = new Resource();
    r.setDirectory(true);

    // Act and Assert
    assertEquals(FileType.DIR, FileType.of(r));
  }

  /**
   * Test FileType {@link FileType#of(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link FileType#of(Resource)}
   */
  @Test
  public void testFileTypeOfWithR_whenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertEquals(FileType.REGULAR_FILE, FileType.of(new MappedResource(r, new CutDirsMapper())));
  }

  /**
   * Test FileType {@link FileType#of(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code REGULAR_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileType#of(Resource)}
   */
  @Test
  public void testFileTypeOfWithR_whenResourceWithName_thenReturnRegularFile() {
    // Arrange, Act and Assert
    assertEquals(FileType.REGULAR_FILE, FileType.of(new Resource("Name")));
  }

  /**
   * Test FileType {@link FileType#of(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code REGULAR_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileType#of(Resource)}
   */
  @Test
  public void testFileTypeOfWithR_whenResource_thenReturnRegularFile() {
    // Arrange, Act and Assert
    assertEquals(FileType.REGULAR_FILE, FileType.of(new Resource()));
  }

  /**
   * Test {@link PermissionUtils#modeFromPermissions(Set, FileType)} with {@code permissions}, {@code type}.
   * <ul>
   *   <li>Given {@code OWNER_EXECUTE}.</li>
   *   <li>Then return {@code 32832}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, FileType)}
   */
  @Test
  public void testModeFromPermissionsWithPermissionsType_givenOwnerExecute_thenReturn32832() {
    // Arrange
    HashSet<PosixFilePermission> permissions = new HashSet<>();
    permissions.add(PosixFilePermission.OWNER_EXECUTE);

    // Act and Assert
    assertEquals(32832, PermissionUtils.modeFromPermissions(permissions, FileType.REGULAR_FILE));
  }

  /**
   * Test {@link PermissionUtils#modeFromPermissions(Set, FileType)} with {@code permissions}, {@code type}.
   * <ul>
   *   <li>Given {@code OWNER_READ}.</li>
   *   <li>Then return {@code 33024}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, FileType)}
   */
  @Test
  public void testModeFromPermissionsWithPermissionsType_givenOwnerRead_thenReturn33024() {
    // Arrange
    HashSet<PosixFilePermission> permissions = new HashSet<>();
    permissions.add(PosixFilePermission.OWNER_READ);

    // Act and Assert
    assertEquals(33024, PermissionUtils.modeFromPermissions(permissions, FileType.REGULAR_FILE));
  }

  /**
   * Test {@link PermissionUtils#modeFromPermissions(Set, FileType)} with {@code permissions}, {@code type}.
   * <ul>
   *   <li>Given {@code OWNER_WRITE}.</li>
   *   <li>Then return {@code 33152}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, FileType)}
   */
  @Test
  public void testModeFromPermissionsWithPermissionsType_givenOwnerWrite_thenReturn33152() {
    // Arrange
    HashSet<PosixFilePermission> permissions = new HashSet<>();
    permissions.add(PosixFilePermission.OWNER_WRITE);
    permissions.add(PosixFilePermission.OWNER_READ);

    // Act and Assert
    assertEquals(33152, PermissionUtils.modeFromPermissions(permissions, FileType.REGULAR_FILE));
  }

  /**
   * Test {@link PermissionUtils#modeFromPermissions(Set, FileType)} with {@code permissions}, {@code type}.
   * <ul>
   *   <li>When {@code DIR}.</li>
   *   <li>Then return {@code 16384}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, FileType)}
   */
  @Test
  public void testModeFromPermissionsWithPermissionsType_whenDir_thenReturn16384() {
    // Arrange, Act and Assert
    assertEquals(16384, PermissionUtils.modeFromPermissions(new HashSet<>(), FileType.DIR));
  }

  /**
   * Test {@link PermissionUtils#modeFromPermissions(Set, FileType)} with {@code permissions}, {@code type}.
   * <ul>
   *   <li>When {@link HashSet#HashSet()}.</li>
   *   <li>Then return {@code 32768}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, FileType)}
   */
  @Test
  public void testModeFromPermissionsWithPermissionsType_whenHashSet_thenReturn32768() {
    // Arrange, Act and Assert
    assertEquals(32768, PermissionUtils.modeFromPermissions(new HashSet<>(), FileType.REGULAR_FILE));
  }

  /**
   * Test {@link PermissionUtils#modeFromPermissions(Set, FileType)} with {@code permissions}, {@code type}.
   * <ul>
   *   <li>When {@code OTHER}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, FileType)}
   */
  @Test
  public void testModeFromPermissionsWithPermissionsType_whenOther_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, PermissionUtils.modeFromPermissions(new HashSet<>(), FileType.OTHER));
  }

  /**
   * Test {@link PermissionUtils#modeFromPermissions(Set, FileType)} with {@code permissions}, {@code type}.
   * <ul>
   *   <li>When {@code SYMLINK}.</li>
   *   <li>Then return {@code 40960}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#modeFromPermissions(Set, FileType)}
   */
  @Test
  public void testModeFromPermissionsWithPermissionsType_whenSymlink_thenReturn40960() {
    // Arrange, Act and Assert
    assertEquals(40960, PermissionUtils.modeFromPermissions(new HashSet<>(), FileType.SYMLINK));
  }

  /**
   * Test {@link PermissionUtils#permissionsFromMode(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#permissionsFromMode(int)}
   */
  @Test
  public void testPermissionsFromMode_whenOne_thenReturnSizeIsOne() {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissionsFromModeResult = PermissionUtils.permissionsFromMode(1);

    // Assert
    assertEquals(1, actualPermissionsFromModeResult.size());
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_EXECUTE));
  }

  /**
   * Test {@link PermissionUtils#permissionsFromMode(int)}.
   * <ul>
   *   <li>When {@link Retryable#RETRY_FOREVER}.</li>
   *   <li>Then return size is nine.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#permissionsFromMode(int)}
   */
  @Test
  public void testPermissionsFromMode_whenRetry_forever_thenReturnSizeIsNine() {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissionsFromModeResult = PermissionUtils
        .permissionsFromMode(Retryable.RETRY_FOREVER);

    // Assert
    assertEquals(9, actualPermissionsFromModeResult.size());
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.GROUP_EXECUTE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.GROUP_READ));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.GROUP_WRITE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_EXECUTE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_READ));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_WRITE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OWNER_EXECUTE));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OWNER_READ));
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OWNER_WRITE));
  }

  /**
   * Test {@link PermissionUtils#permissionsFromMode(int)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#permissionsFromMode(int)}
   */
  @Test
  public void testPermissionsFromMode_whenTwo_thenReturnSizeIsOne() {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissionsFromModeResult = PermissionUtils.permissionsFromMode(2);

    // Assert
    assertEquals(1, actualPermissionsFromModeResult.size());
    assertTrue(actualPermissionsFromModeResult.contains(PosixFilePermission.OTHERS_WRITE));
  }

  /**
   * Test {@link PermissionUtils#getPermissions(Resource, Function)}.
   * <ul>
   *   <li>Given Property is {@code java.io.tmpdir} is array of {@link String} with {@code foo} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#getPermissions(Resource, Function)}
   */
  @Test
  public void testGetPermissions_givenPropertyIsJavaIoTmpdirIsArrayOfStringWithFooToFile() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile();

    // Act
    Set<PosixFilePermission> actualPermissions = PermissionUtils.getPermissions(new BZip2Resource(), null);

    // Assert
    assertTrue(actualPermissions.isEmpty());
  }

  /**
   * Test {@link PermissionUtils#getPermissions(Resource, Function)}.
   * <ul>
   *   <li>Then return size is seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#getPermissions(Resource, Function)}
   */
  @Test
  public void testGetPermissions_thenReturnSizeIsSeven() throws IOException {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissions = PermissionUtils
        .getPermissions(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()), null);

    // Assert
    assertEquals(7, actualPermissions.size());
    assertTrue(actualPermissions.contains(PosixFilePermission.GROUP_EXECUTE));
    assertTrue(actualPermissions.contains(PosixFilePermission.GROUP_READ));
    assertTrue(actualPermissions.contains(PosixFilePermission.OTHERS_EXECUTE));
    assertTrue(actualPermissions.contains(PosixFilePermission.OTHERS_READ));
    assertTrue(actualPermissions.contains(PosixFilePermission.OWNER_EXECUTE));
    assertTrue(actualPermissions.contains(PosixFilePermission.OWNER_READ));
    assertTrue(actualPermissions.contains(PosixFilePermission.OWNER_WRITE));
  }

  /**
   * Test {@link PermissionUtils#getPermissions(Resource, Function)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PermissionUtils#getPermissions(Resource, Function)}
   */
  @Test
  public void testGetPermissions_whenResource_thenReturnEmpty() throws IOException {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissions = PermissionUtils.getPermissions(new Resource(), null);

    // Assert
    assertTrue(actualPermissions.isEmpty());
  }
}
