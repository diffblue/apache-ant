package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class OsDiffblueTest {
  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os()} Arch is {@code Arch}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsArchIsArch_thenReturnFalse() throws BuildException {
    // Arrange
    Os os = new Os();
    os.setArch("Arch");

    // Act and Assert
    assertFalse(os.eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os()} Name is {@code mac os x}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsNameIsMacOsX_thenReturnTrue() throws BuildException {
    // Arrange
    Os os = new Os();
    os.setName("mac os x");

    // Act and Assert
    assertTrue(os.eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os()} Name is {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsNameIsName_thenReturnFalse() throws BuildException {
    // Arrange
    Os os = new Os();
    os.setName("Name");

    // Act and Assert
    assertFalse(os.eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os()} Version is {@code 1.0.2}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsVersionIs102_thenReturnFalse() throws BuildException {
    // Arrange
    Os os = new Os();
    os.setVersion("1.0.2");

    // Act and Assert
    assertFalse(os.eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with family is {@link Os#FAMILY_DOS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamilyIsFamily_dos_thenReturnFalse() throws BuildException {
    // Arrange, Act and Assert
    assertFalse((new Os(Os.FAMILY_DOS)).eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with family is {@link Os#FAMILY_MAC}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamilyIsFamily_mac_thenReturnTrue() throws BuildException {
    // Arrange, Act and Assert
    assertTrue((new Os(Os.FAMILY_MAC)).eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with family is {@link Os#FAMILY_NETWARE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamilyIsFamily_netware_thenReturnFalse() throws BuildException {
    // Arrange, Act and Assert
    assertFalse((new Os(Os.FAMILY_NETWARE)).eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with family is {@link Os#FAMILY_OS2}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamilyIsFamily_os2_thenReturnFalse() throws BuildException {
    // Arrange, Act and Assert
    assertFalse((new Os(Os.FAMILY_OS2)).eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with family is {@link Os#FAMILY_VMS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamilyIsFamily_vms_thenReturnFalse() throws BuildException {
    // Arrange, Act and Assert
    assertFalse((new Os(Os.FAMILY_VMS)).eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with family is {@link Os#FAMILY_WINDOWS} Arch is {@link Os#FAMILY_WINDOWS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamilyIsFamily_windowsArchIsFamily_windows_thenReturnFalse() throws BuildException {
    // Arrange
    Os os = new Os(Os.FAMILY_WINDOWS);
    os.setArch(Os.FAMILY_WINDOWS);

    // Act and Assert
    assertFalse(os.eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with family is {@link Os#FAMILY_WINDOWS} Name is {@link Os#FAMILY_WINDOWS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamilyIsFamily_windowsNameIsFamily_windows_thenReturnFalse() throws BuildException {
    // Arrange
    Os os = new Os(Os.FAMILY_WINDOWS);
    os.setName(Os.FAMILY_WINDOWS);

    // Act and Assert
    assertFalse(os.eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with family is {@link Os#FAMILY_WINDOWS} Version is {@code 1.0.2}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamilyIsFamily_windowsVersionIs102_thenReturnFalse() throws BuildException {
    // Arrange
    Os os = new Os(Os.FAMILY_WINDOWS);
    os.setVersion("1.0.2");

    // Act and Assert
    assertFalse(os.eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with family is {@link Os#FAMILY_WINDOWS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamilyIsFamily_windows_thenReturnFalse() throws BuildException {
    // Arrange, Act and Assert
    assertFalse((new Os(Os.FAMILY_WINDOWS)).eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os(String)} with {@code Family}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOsWithFamily_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Os("Family")).eval());
  }

  /**
   * Test {@link Os#eval()}.
   * <ul>
   *   <li>Given {@link Os#Os()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#eval()}
   */
  @Test
  public void testEval_givenOs_thenReturnFalse() throws BuildException {
    // Arrange, Act and Assert
    assertFalse((new Os()).eval());
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_9X}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_9x_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(Os.FAMILY_9X));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_DOS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_dos_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(Os.FAMILY_DOS));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_MAC}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_mac_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Os.isFamily(Os.FAMILY_MAC));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_NETWARE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_netware_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(Os.FAMILY_NETWARE));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_NT}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_nt_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(Os.FAMILY_NT));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_OS2}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_os2_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(Os.FAMILY_OS2));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_OS400}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_os400_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(Os.FAMILY_OS400));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_TANDEM}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_tandem_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(Os.FAMILY_TANDEM));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@code Family}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> Os.isFamily("Family"));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_UNIX}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_unix_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Os.isFamily(Os.FAMILY_UNIX));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_WINDOWS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_windows_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(Os.FAMILY_WINDOWS));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_ZOS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenFamily_zos_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(Os.FAMILY_ZOS));
  }

  /**
   * Test {@link Os#isFamily(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isFamily(String)}
   */
  @Test
  public void testIsFamily_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isFamily(null));
  }

  /**
   * Test {@link Os#isName(String)}.
   * <ul>
   *   <li>When {@code mac os x}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isName(String)}
   */
  @Test
  public void testIsName_whenMacOsX_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Os.isName("mac os x"));
  }

  /**
   * Test {@link Os#isName(String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isName(String)}
   */
  @Test
  public void testIsName_whenName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isName("Name"));
  }

  /**
   * Test {@link Os#isName(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isName(String)}
   */
  @Test
  public void testIsName_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isName(null));
  }

  /**
   * Test {@link Os#isArch(String)}.
   * <ul>
   *   <li>When {@code Arch}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isArch(String)}
   */
  @Test
  public void testIsArch_whenArch_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isArch("Arch"));
  }

  /**
   * Test {@link Os#isArch(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isArch(String)}
   */
  @Test
  public void testIsArch_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isArch(null));
  }

  /**
   * Test {@link Os#isArch(String)}.
   * <ul>
   *   <li>When Property is {@code os.arch}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isArch(String)}
   */
  @Test
  public void testIsArch_whenPropertyIsOsArch_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Os.isArch(System.getProperty("os.arch")));
  }

  /**
   * Test {@link Os#isVersion(String)}.
   * <ul>
   *   <li>When {@code 1.0.2}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isVersion(String)}
   */
  @Test
  public void testIsVersion_when102_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isVersion("1.0.2"));
  }

  /**
   * Test {@link Os#isVersion(String)}.
   * <ul>
   *   <li>When {@code 15.2}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isVersion(String)}
   */
  @Test
  public void testIsVersion_when152_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Os.isVersion("15.2"));
  }

  /**
   * Test {@link Os#isVersion(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isVersion(String)}
   */
  @Test
  public void testIsVersion_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isVersion(null));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@code 15.2}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_when152_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Os.isOs(null, "mac os x", System.getProperty("os.arch"), "15.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_DOS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenFamily_dos_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(Os.FAMILY_DOS, "Name", "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_MAC}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenFamily_mac_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(Os.FAMILY_MAC, "Name", "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_NETWARE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenFamily_netware_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(Os.FAMILY_NETWARE, "Name", "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@code Family}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenFamily_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> Os.isOs("Family", "Name", "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_VMS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenFamily_vms_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(Os.FAMILY_VMS, "Name", "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_WINDOWS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenFamily_windows_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(Os.FAMILY_WINDOWS, "Name", "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_WINDOWS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenFamily_windows_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(Os.FAMILY_WINDOWS, null, "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_WINDOWS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenFamily_windows_thenReturnFalse3() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(Os.FAMILY_WINDOWS, "Name", null, "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@link Os#FAMILY_WINDOWS}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenFamily_windows_thenReturnFalse4() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(Os.FAMILY_WINDOWS, "Name", "Arch", null));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@code mac os x}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenMacOsX_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(null, "mac os x", "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(null, "Name", "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(null, null, "Arch", "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenNull_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(null, null, null, "1.0.2"));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenNull_thenReturnFalse3() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(null, null, null, null));
  }

  /**
   * Test {@link Os#isOs(String, String, String, String)}.
   * <ul>
   *   <li>When Property is {@code os.arch}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Os#isOs(String, String, String, String)}
   */
  @Test
  public void testIsOs_whenPropertyIsOsArch_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Os.isOs(null, "mac os x", System.getProperty("os.arch"), "1.0.2"));
  }
}
