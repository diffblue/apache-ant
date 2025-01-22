package org.apache.tools.ant.types.spi;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.IOException;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class ServiceDiffblueTest {
  /**
   * Test {@link Service#setProvider(String)}.
   * <p>
   * Method under test: {@link Service#setProvider(String)}
   */
  @Test
  public void testSetProvider() throws IOException {
    // Arrange
    Service service = new Service();

    // Act
    service.setProvider("Class Name");

    // Assert
    byte[] byteArray = new byte[10];
    assertEquals(10, service.getAsStream().read(byteArray));
    assertArrayEquals("Class Name".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Service#addConfiguredProvider(Provider)}.
   * <ul>
   *   <li>Given {@code Provider}.</li>
   *   <li>Then {@link Service} (default constructor) AsStream read is eight.</li>
   * </ul>
   * <p>
   * Method under test: {@link Service#addConfiguredProvider(Provider)}
   */
  @Test
  public void testAddConfiguredProvider_givenProvider_thenServiceAsStreamReadIsEight() throws IOException {
    // Arrange
    Service service = new Service();

    Provider provider = new Provider();
    provider.setClassName("Provider");

    // Act
    service.addConfiguredProvider(provider);

    // Assert
    byte[] byteArray = new byte[8];
    assertEquals(8, service.getAsStream().read(byteArray));
    assertArrayEquals("Provider".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Service#setType(String)}
   *   <li>{@link Service#getType()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Service service = new Service();

    // Act
    service.setType("Type");

    // Assert
    assertEquals("Type", service.getType());
  }

  /**
   * Test {@link Service#getAsStream()}.
   * <ul>
   *   <li>Given {@link Provider} (default constructor) ClassName is {@code Type}.</li>
   *   <li>Then return read is four.</li>
   * </ul>
   * <p>
   * Method under test: {@link Service#getAsStream()}
   */
  @Test
  public void testGetAsStream_givenProviderClassNameIsType_thenReturnReadIsFour() throws IOException {
    // Arrange
    Provider provider = new Provider();
    provider.setClassName("Type");

    Service service = new Service();
    service.addConfiguredProvider(provider);

    // Act and Assert
    byte[] byteArray = new byte[4];
    assertEquals(4, service.getAsStream().read(byteArray));
    assertArrayEquals("Type".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Service#getAsStream()}.
   * <ul>
   *   <li>Given {@link Provider} (default constructor) ClassName is {@code Type}.</li>
   *   <li>Then return read is nine.</li>
   * </ul>
   * <p>
   * Method under test: {@link Service#getAsStream()}
   */
  @Test
  public void testGetAsStream_givenProviderClassNameIsType_thenReturnReadIsNine() throws IOException {
    // Arrange
    Provider provider = new Provider();
    provider.setClassName("Type");

    Provider provider2 = new Provider();
    provider2.setClassName("Type");

    Service service = new Service();
    service.addConfiguredProvider(provider2);
    service.addConfiguredProvider(provider);

    // Act and Assert
    byte[] byteArray = new byte[9];
    assertEquals(9, service.getAsStream().read(byteArray));
    assertArrayEquals("Type\nType".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Service#getAsStream()}.
   * <ul>
   *   <li>Given {@link Service} (default constructor).</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Service#getAsStream()}
   */
  @Test
  public void testGetAsStream_givenService_thenReturnReadIsMinusOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new Service()).getAsStream().read(new byte[]{}));
  }

  /**
   * Test {@link Service#check()}.
   * <ul>
   *   <li>Given {@link Service} (default constructor) Type is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Service#check()}
   */
  @Test
  public void testCheck_givenServiceTypeIsEmptyString_thenThrowBuildException() {
    // Arrange
    Service service = new Service();
    service.setType("");

    // Act and Assert
    assertThrows(BuildException.class, () -> service.check());
  }

  /**
   * Test {@link Service#check()}.
   * <ul>
   *   <li>Given {@link Service} (default constructor) Type is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Service#check()}
   */
  @Test
  public void testCheck_givenServiceTypeIsFoo_thenThrowBuildException() {
    // Arrange
    Service service = new Service();
    service.setType("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> service.check());
  }

  /**
   * Test {@link Service#check()}.
   * <ul>
   *   <li>Given {@link Service} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Service#check()}
   */
  @Test
  public void testCheck_givenService_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Service()).check());
  }

  /**
   * Test new {@link Service} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Service}
   */
  @Test
  public void testNewService() throws IOException {
    // Arrange and Act
    Service actualService = new Service();

    // Assert
    Location location = actualService.getLocation();
    assertNull(location.getFileName());
    assertNull(actualService.getDescription());
    assertNull(actualService.getType());
    assertNull(actualService.getProject());
    assertEquals(-1, actualService.getAsStream().read(new byte[]{}));
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
