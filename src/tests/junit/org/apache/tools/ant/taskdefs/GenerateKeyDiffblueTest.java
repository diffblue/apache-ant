package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.GenerateKey.DistinguishedName;
import org.apache.tools.ant.taskdefs.GenerateKey.DnameParam;
import org.junit.Test;

public class GenerateKeyDiffblueTest {
  /**
   * Test {@link GenerateKey#createDname()}.
   * <ul>
   *   <li>Given {@link GenerateKey} (default constructor) Dname is {@code Dname}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenerateKey#createDname()}
   */
  @Test
  public void testCreateDname_givenGenerateKeyDnameIsDname_thenThrowBuildException() throws BuildException {
    // Arrange
    GenerateKey generateKey = new GenerateKey();
    generateKey.setDname("Dname");

    // Act and Assert
    assertThrows(BuildException.class, () -> generateKey.createDname());
  }

  /**
   * Test DistinguishedName {@link DistinguishedName#createParam()}.
   * <p>
   * Method under test: {@link DistinguishedName#createParam()}
   */
  @Test
  public void testDistinguishedNameCreateParam() {
    // Arrange and Act
    Object actualCreateParamResult = (new DistinguishedName()).createParam();

    // Assert
    assertTrue(actualCreateParamResult instanceof DnameParam);
    assertNull(((DnameParam) actualCreateParamResult).getName());
    assertNull(((DnameParam) actualCreateParamResult).getValue());
    assertFalse(((DnameParam) actualCreateParamResult).isComplete());
  }

  /**
   * Test DistinguishedName {@link DistinguishedName#encode(String)}.
   * <p>
   * Method under test: {@link DistinguishedName#encode(String)}
   */
  @Test
  public void testDistinguishedNameEncode() {
    // Arrange, Act and Assert
    assertEquals("String", (new DistinguishedName()).encode("String"));
  }

  /**
   * Test DistinguishedName getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link DistinguishedName}
   *   <li>{@link DistinguishedName#toString()}
   * </ul>
   */
  @Test
  public void testDistinguishedNameGettersAndSetters() {
    // Arrange, Act and Assert
    assertEquals("", (new DistinguishedName()).toString());
  }

  /**
   * Test DnameParam getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link DnameParam}
   *   <li>{@link DnameParam#setName(String)}
   *   <li>{@link DnameParam#setValue(String)}
   *   <li>{@link DnameParam#getName()}
   *   <li>{@link DnameParam#getValue()}
   * </ul>
   */
  @Test
  public void testDnameParamGettersAndSetters() {
    // Arrange and Act
    DnameParam actualDnameParam = new DnameParam();
    actualDnameParam.setName(Manifest.ATTRIBUTE_NAME);
    actualDnameParam.setValue("42");
    String actualName = actualDnameParam.getName();

    // Assert
    assertEquals("42", actualDnameParam.getValue());
    assertEquals(Manifest.ATTRIBUTE_NAME, actualName);
  }

  /**
   * Test DnameParam {@link DnameParam#isComplete()}.
   * <ul>
   *   <li>Given {@link DnameParam} (default constructor) Value is {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DnameParam#isComplete()}
   */
  @Test
  public void testDnameParamIsComplete_givenDnameParamValueIsFoo_thenReturnTrue() {
    // Arrange
    DnameParam dnameParam = new DnameParam();
    dnameParam.setName("foo");
    dnameParam.setValue("foo");

    // Act and Assert
    assertTrue(dnameParam.isComplete());
  }

  /**
   * Test DnameParam {@link DnameParam#isComplete()}.
   * <ul>
   *   <li>Given {@link DnameParam} (default constructor) Value is {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DnameParam#isComplete()}
   */
  @Test
  public void testDnameParamIsComplete_givenDnameParamValueIsNull_thenReturnFalse() {
    // Arrange
    DnameParam dnameParam = new DnameParam();
    dnameParam.setName("foo");
    dnameParam.setValue(null);

    // Act and Assert
    assertFalse(dnameParam.isComplete());
  }

  /**
   * Test DnameParam {@link DnameParam#isComplete()}.
   * <ul>
   *   <li>Given {@link DnameParam} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DnameParam#isComplete()}
   */
  @Test
  public void testDnameParamIsComplete_givenDnameParam_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new DnameParam()).isComplete());
  }

  /**
   * Test {@link GenerateKey#setDname(String)}.
   * <p>
   * Method under test: {@link GenerateKey#setDname(String)}
   */
  @Test
  public void testSetDname() {
    // Arrange
    GenerateKey generateKey = new GenerateKey();

    // Act
    generateKey.setDname("Dname");

    // Assert
    assertEquals("Dname", generateKey.dname);
  }

  /**
   * Test {@link GenerateKey#setKeysize(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link GenerateKey} (default constructor) {@link GenerateKey#keysize} is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenerateKey#setKeysize(String)}
   */
  @Test
  public void testSetKeysize_when42_thenGenerateKeyKeysizeIsFortyTwo() throws BuildException {
    // Arrange
    GenerateKey generateKey = new GenerateKey();

    // Act
    generateKey.setKeysize("42");

    // Assert
    assertEquals(42, generateKey.keysize);
  }

  /**
   * Test {@link GenerateKey#setKeysize(String)}.
   * <ul>
   *   <li>When {@code Keysize}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenerateKey#setKeysize(String)}
   */
  @Test
  public void testSetKeysize_whenKeysize_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new GenerateKey()).setKeysize("Keysize"));
  }

  /**
   * Test {@link GenerateKey#setValidity(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link GenerateKey} (default constructor) {@link GenerateKey#validity} is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenerateKey#setValidity(String)}
   */
  @Test
  public void testSetValidity_when42_thenGenerateKeyValidityIsFortyTwo() throws BuildException {
    // Arrange
    GenerateKey generateKey = new GenerateKey();

    // Act
    generateKey.setValidity("42");

    // Assert
    assertEquals(42, generateKey.validity);
  }

  /**
   * Test {@link GenerateKey#setValidity(String)}.
   * <ul>
   *   <li>When {@code Validity}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenerateKey#setValidity(String)}
   */
  @Test
  public void testSetValidity_whenValidity_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new GenerateKey()).setValidity("Validity"));
  }

  /**
   * Test {@link GenerateKey#execute()}.
   * <ul>
   *   <li>Given {@link GenerateKey} (default constructor) Alias is {@link SignJar#ERROR_NO_ALIAS}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenerateKey#execute()}
   */
  @Test
  public void testExecute_givenGenerateKeyAliasIsError_no_alias_thenThrowBuildException() throws BuildException {
    // Arrange
    GenerateKey generateKey = new GenerateKey();
    generateKey.setAlias(SignJar.ERROR_NO_ALIAS);

    // Act and Assert
    assertThrows(BuildException.class, () -> generateKey.execute());
  }

  /**
   * Test {@link GenerateKey#execute()}.
   * <ul>
   *   <li>Given {@link GenerateKey} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenerateKey#execute()}
   */
  @Test
  public void testExecute_givenGenerateKey_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new GenerateKey()).execute());
  }

  /**
   * Test {@link GenerateKey#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenerateKey#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    GenerateKey generateKey = new GenerateKey();
    generateKey.setStorepass(SignJar.ERROR_NO_STOREPASS);
    generateKey.setAlias(SignJar.ERROR_NO_ALIAS);

    // Act and Assert
    assertThrows(BuildException.class, () -> generateKey.execute());
  }

  /**
   * Test new {@link GenerateKey} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link GenerateKey}
   */
  @Test
  public void testNewGenerateKey() {
    // Arrange and Act
    GenerateKey actualGenerateKey = new GenerateKey();

    // Assert
    assertNull(actualGenerateKey.getDescription());
    assertNull(actualGenerateKey.getTaskName());
    assertNull(actualGenerateKey.getTaskType());
    assertNull(actualGenerateKey.alias);
    assertNull(actualGenerateKey.dname);
    assertNull(actualGenerateKey.keyalg);
    assertNull(actualGenerateKey.keypass);
    assertNull(actualGenerateKey.keystore);
    assertNull(actualGenerateKey.saname);
    assertNull(actualGenerateKey.sigalg);
    assertNull(actualGenerateKey.storepass);
    assertNull(actualGenerateKey.storetype);
    assertNull(actualGenerateKey.getProject());
    assertNull(actualGenerateKey.getOwningTarget());
    assertNull(actualGenerateKey.expandedDname);
    assertEquals(0, actualGenerateKey.keysize);
    assertEquals(0, actualGenerateKey.validity);
    assertFalse(actualGenerateKey.verbose);
  }
}
